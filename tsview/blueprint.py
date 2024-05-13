import io
import json
import traceback
from contextlib import redirect_stdout
import pandas as pd
from flask import (
    Blueprint,
    jsonify,
    make_response,
    request,
    render_template,
    url_for
)
from pml import HTML

from dash import _utils

from psyl.lisp import (
    parse as fparse,
    serialize,
)

from rework_ui.helper import argsdict as _args

from sqlhelp import select

from tshistory import search
from tshistory.util import find_first_uriname

from tshistory_formula.helper import validate_formula
from tshistory_formula.registry import FUNCS
from tshistory_formula.interpreter import jsontypes

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
        appname = find_first_uriname()
        instance = "Local" if len(appname)==0 else appname.capitalize()
        import tshistory_refinery
        version = tshistory_refinery.__version__
        return render_template(
            'homepage.html',
            flags=json.dumps([baseurl, instance, version]),
        )

    @bp.route('/tsview')
    def home():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        flags_menu = json.dumps([homeurl(), 'timeseries-quickview'])
        return render_template(
            'tsview.html',
            homeurl=homeurl(),
            haseditor=json.dumps(False),  # NOTE: really fix me
            series=request.args.getlist("series"),
            flags_menu=flags_menu,
        )

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
            flags_menu = json.dumps([homeurl(), 'timeseries-delete'])
            return render_template(
                'tsedit.html',
                edit_kind="Delete",
                homeurl=homeurl(),
                flags_menu=flags_menu,
            )

        return 'You do not have the delete capability.'

    @bp.route('/formulacache')
    def formulacache():
        if has_roles('admin', 'rw', 'ro'):
            flags_menu = json.dumps([homeurl(), 'formula-cache'])
            return render_template(
                'cache.html',
                homeurl=homeurl(),
                flags_menu=flags_menu,
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
        flags_menu = json.dumps([homeurl(), 'formula-create'])
        return render_template(
            'tsformula.html',
            homeurl=homeurl(),
            spec=spec(),
            formula=json.dumps(formula),
            flags_menu=flags_menu
        )

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

    @bp.route('/tsformula/spec-operators')
    def serve_operators():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        funcs = {
            name: (FUNCS[name].__doc__,
                   FUNCS[name].__module__)
            for name in sorted(FUNCS)
            if FUNCS[name].__doc__ is not None
        }
        to_send = []
        for (name, (doc, source)) in funcs.items():
            to_send.append(
                {
                    'name': name,
                    'doc': doc,
                    'source': source
                }
            )
        return json.dumps(to_send)

    @bp.route('/tsformula/operators')
    def formula_operators():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        home_url = homeurl() or "/"
        flags_menu = json.dumps([home_url, 'formula-documentation'])
        return render_template(
            'operators.html',
            baseurl=home_url,
            flags_menu=flags_menu,
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

    @bp.route('/addformulas')
    def addformulas():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        baseurl = homeurl()
        flags_menu = json.dumps([homeurl(), 'formula-batch'])
        return render_template(
            'addformulas.html',
            baseurl=baseurl,
            flags_menu=flags_menu,
        )

    @bp.route('/updateformulas', methods=['PUT'])
    def updateformulas():
        if not request.files:
            return jsonify({'errors': ['Missing CSV file']})

        args = _args(request.form)
        stdout = io.StringIO()
        output = []
        try:
            content = request.files.to_dict()['new_formula.csv'].stream.read().decode("utf-8")
            stdout.write(content)
            stdout.seek(0)
            df_formula = pd.read_csv(stdout, dtype={'name': str, 'text': str}, sep=',')
            if ('name' not in df_formula.columns) or ('text' not in df_formula.columns):
                return jsonify({
                    'status' : 'invalid',
                    'errors': {'syntax' : ["Invalid file. Requested columns are ['name', 'text']"]},
                    'warnings': {},
                    'crash' : '',
                    'output' : {}
                })
            errors, warnings = validate_formula(tsa, df_formula)
            if errors or not args.reallydoit:
                status = 'invalid' if errors else 'valid'
                print(jsonify({
                    'status': status,
                    'errors': errors,
                    'warnings': warnings,
                    'crash' : '',
                    'output' : {}
                }))
                return jsonify({
                    'status': status,
                    'errors': errors,
                    'warnings': warnings,
                    'crash' : '',
                    'output' : {}
                })

            with redirect_stdout(stdout):
                for row in df_formula.itertuples():
                    tsa.register_formula(
                        row.name,
                        row.text,
                        reject_unknown=False
                    )
                    output.append(row.name + ' : ' + row.text)

        except Exception:
            traceback.print_exc()
            h = HTML()
            return json.dumps({
                'status': 'fail',
                'crash': str(h(traceback.format_exc())),
                'output': {},
                'errors' : {},
                'warnings' : {}
            })
        return jsonify({
            'status': 'saved',
            'output': {'Registered' : output},
            'crash': '',
            'errors' : {},
            'warnings' : {}
        })

    @bp.route('/downloadformulas')
    def downloadformulas():
        formulas = pd.read_sql(
            'select name, internal_metadata->\'formula\' as text '
            'from tsh.registry '
            'where internal_metadata->\'formula\' is not null',
            tsa.engine
        )
        df = formulas.sort_values(
            by=['name', 'text'],
            kind='mergesort'
        )
        df['text'] = df['text'].apply(lambda x: serialize(fparse(x)))
        response = make_response(
            df.to_csv(
                index=False,
                quotechar="'"
            ), 200
        )
        response.headers['Content-Type'] = 'text/json'

        return response

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
        flags_menu = json.dumps([homeurl(), 'basket-edit'])
        return render_template(
            'queryeditor.html',
            homeurl=homeurl(),
            flags_menu=flags_menu,
        )

    # info

    @bp.route('/tsinfo')
    def tsinfo():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        flags_menu = json.dumps([homeurl(), 'timeseries-catalog'])
        return render_template(
            'tsinfo.html',
            homeurl=homeurl(),
            name=request.args.get('name'),
            flags_menu=flags_menu,
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
        base_url = homeurl()
        flags_menu = json.dumps([base_url, 'timeseries-catalog'])
        return render_template(
            'tssearch.html',
            homeurl=homeurl(),
            flags_menu=flags_menu,
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

        name = request.args.get('name')
        min = request.args.get('startdate')
        max = request.args.get('enddate')
        flags_menu = json.dumps([homeurl(), 'timeseries-catalog'])
        return render_template(
            'tseditor.html',
            homeurl=homeurl(),
            name=name,
            min=min,
            max=max,
            flags_menu=flags_menu,
        )

    @bp.route('/formula-components/<seriesname>')
    def formula_components(seriesname):
        assert tsa.exists(seriesname)
        formula = tsa.formula(seriesname)
        tree = fparse(formula)
        infos = [
            {
                'name': name,
                'type': tsa.type(name)
            }
            for name, expr in tsa.tsh.find_series(tsa.engine, tree).items()
        ]
        return json.dumps(infos)

    # menu

    @bp.route('/menu')
    def serve_menu():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        full_url = url_for('tsview.home', _external=True)
        full_url = full_url[:full_url.rindex('/')]
        dashboard_url = full_url.replace('refinery', 'dashboard')
        return json.dumps(
            menu_spec(
                dashboard_url=dashboard_url
            )
        )

    @bp.route('/icons')
    def serve_icons():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        return json.dumps(icons_definition)

    @bp.route('/formulas')
    def formulas():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        return render_template(
            'formulas.html',
            homeurl=homeurl(),
            name=request.args.get('name')
        )

    return bp
