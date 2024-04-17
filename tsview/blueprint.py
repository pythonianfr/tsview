import json
import pandas as pd
from flask import Blueprint, jsonify, request, render_template, url_for

from dash import _utils

from sqlhelp import select

from tshistory import search

from tshistory_formula.registry import FUNCS
from tshistory_formula.interpreter import jsontypes

import tshistory_refinery

from tsview.util import (
    argsdict as _argsdict,
    format_formula
)
from tsview.menu import definition as menu_spec
from tsview.icons import definition as icons_definition


# monkeypatch dash utility
def set_read_only(self, names, msg='Attribute is read-only'):
    return

_utils.AttributeDict.set_read_only = set_read_only


def primary_names(tsa):
    cat = list(tsa.catalog(
        allsources=False
    ).values())[0]
    return sorted(
        name
        for name, kind in cat
        if kind == 'primary'
    )


def homeurl():
    homeurl = url_for('tsview.home')
    baseurl = homeurl[:homeurl.rindex('/')]
    if len(baseurl):
        return baseurl
    return baseurl


def has_roles(*required_roles):
    role = request.environ.get('ROLE') or 'guest'
    return role in required_roles


def tsview(tsa):
    bp = Blueprint(
        'tsview',
        __name__,
        template_folder='tsview_templates',
        static_folder='tsview_static',
    )

    @bp.route('/')
    def homepage():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        baseurl = homeurl()
        instance = "Local" if len(baseurl)==0 else baseurl.split("refinery.")[1].split(".pythonian")[0]
        version = tshistory_refinery.__version__
        return render_template(
            'homepage.html',
            flags=json.dumps([baseurl, instance, version]),
        )

    @bp.route('/tsview')
    def home():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        return render_template('tsview.html',
                               homeurl=homeurl(),
                               haseditor=json.dumps(False),  # NOTE: really fix me
                               series=request.args.getlist("series"))

    class logargs(_argsdict):
        defaults = {
            'series': None,
            'seriesvocab': lambda: primary_names(tsa)
        }
        types = {
            'series': str,
        }

    @bp.route('/tsviewlog')
    def tsviewlog():
        args = logargs(request.args)
        return render_template('tslog.html', **args)

    @bp.route('/tslog')
    def tslog():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        args = logargs(request.args)
        log = tsa.log(
            name=args.series
        )

        if not log:
            return 'No result.'

        return pd.DataFrame(reversed(log)).to_html(index=False)

    @bp.route('/tsdelete')
    def tsdelete():
        if has_roles('admin', 'rw'):
            return render_template('tsedit.html',
                                   edit_kind="Delete",
                                   homeurl=homeurl())

        return 'You do not have the delete capability.'

    @bp.route('/formulacache')
    def formulacache():
        if has_roles('admin', 'rw', 'ro'):
            return render_template(
                'cache.html',
                homeurl=homeurl()
            )

    # formula editor

    @bp.route('/spec')
    def spec():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        return json.dumps(
            sorted(
                [
                    (op_name, list(op_spec.items()))
                    for op_name, op_spec in json.loads(jsontypes()).items()
                ],
                key=lambda x: ("\x00",) if x[0] == 'series' else x
            )
        )

    @bp.route('/tsformula')
    def tsformula():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        name = request.args.get('name')
        formula = None
        if name:
            formula = dict(
                name=name,
                code=tsa.formula(name) or ''
            )
        return render_template('tsformula.html',
                               homeurl=homeurl(),
                               spec=spec(),
                               formula=json.dumps(formula))

    @bp.route('/tsformula/pygmentize', methods=['POST'])
    def tsformula_pygmentize():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        return json.dumps(
            format_formula(
                request.data.decode('utf-8'),
                baseurl=homeurl()
            )
        )

    @bp.route('/tsformula/operators')
    def formula_operators():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        return render_template(
            'operators.html',
            funcs={
                name: (FUNCS[name].__doc__,
                       FUNCS[name].__module__)
                for name in sorted(FUNCS)
                if FUNCS[name].__doc__ is not None
            }
        )

    @bp.route('/tsformula/try')
    def tryformula():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        # here we take a big bad shortcut ...
        # we want to think on how to expose that
        # in tshistory rest api
        # and for all sources ...
        # for now we are single source only

        return tsa.eval_formula(
            request.args['formula']
        ).to_json(
            orient='index',
            date_format='iso'
        )

    # query editor

    @bp.route('/queryspec')
    def queryspec():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        types = {}
        for lispname, kname in search._OPMAP.items():
            types[lispname] = search.query.klassbyname(kname).__sig__()

        return jsonify(types)

    @bp.route('/queryeditor')
    def queryeditor():
        if not has_roles('admin', 'rw'):
            return 'Nothing to see there.'

        return render_template(
            'queryeditor.html',
            homeurl=homeurl()
        )

    # info

    @bp.route('/tsinfo')
    def tsinfo():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        return render_template(
            'tsinfo.html',
            homeurl=homeurl(),
            name=request.args.get('name')
        )

    @bp.route('/groupinfo')
    def groupinfo():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        return render_template(
            'groupinfo.html',
            homeurl=homeurl(),
            name=request.args.get('name')
        )

    @bp.route('/tsinfo/canwrite')
    def canwrite():
        return json.dumps(
            has_roles('admin', 'rw')
        )

    # catalog

    @bp.route('/tssearch')
    def tssearch():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        return render_template(
            'tssearch.html',
            homeurl=homeurl()
        )

    @bp.route('/tssearch/allmetadata')
    def all_series_metadata():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        # here we take a big bad shortcut ...
        # we want to think on how to expose that
        # in tshistory rest api
        # and for all sources ...
        # for now we are single source only
        engine = tsa.engine

        q = select(
            'name', 'internal_metadata', 'metadata'
        ).table(
            f'"{tsa.namespace}".registry'
        )

        m = {
            name: imeta | (meta or {})
            for name, imeta, meta in q.do(engine).fetchall()
        }

        return jsonify(m)

    @bp.route('/tssearch/allformula')
    def all_series_formulas():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        # here we take a big bad shortcut ...
        # we want to think on how to expose that
        # in tshistory rest api
        # and for all sources ...
        # for now we are single source only
        engine = tsa.engine

        q = select(
            'name', 'internal_metadata->\'formula\''
        ).table(
            f'"{tsa.namespace}".registry'
        ).where(
            'internal_metadata->\'formula\' is not null'
        )

        return jsonify(
            {
                name: formula
                for name, formula in q.do(engine).fetchall()
            }
        )

    @bp.route('/groupsearch/allformula')
    def all_group_formulas():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        engine = tsa.engine
        q = select(
            'name', 'internal_metadata->\'formula\''
        ).table(
            f'"{tsa.namespace}".group_registry'
        ).where(
            'internal_metadata->\'formula\' is not null'
        )

        return jsonify(
            {
                name: formula
                for name, formula in q.do(engine).fetchall()
            }
        )

    @bp.route('/groupsearch/allmetadata')
    def all_group_metadata():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        engine = tsa.engine

        q = select(
            'name', 'internal_metadata', 'metadata'
        ).table(
            f'"{tsa.namespace}".group_registry'
        )

        m = {
            name: imeta | (meta or {})
            for name, imeta, meta in q.do(engine).fetchall()
        }

        return jsonify(
            dict(m)
        )

    @bp.route('/tseditor')
    def tseditor():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        return render_template(
            'tseditor.html',
            homeurl=homeurl(),
            name=request.args.get('name')
        )

    # menu

    @bp.route('/menu')
    def serve_menu():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        return json.dumps(menu_spec)

    @bp.route('/icons')
    def serve_icons():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        return json.dumps(icons_definition)

    return bp
