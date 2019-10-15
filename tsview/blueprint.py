import pandas as pd
from flask import Blueprint, request, render_template, url_for

from dash import _utils

from tshistory.tsio import timeseries

from tsview.util import argsdict as _argsdict


# monkeypatch dash utility
def set_read_only(self, names, msg='Attribute is read-only'):
    return

_utils.AttributeDict.set_read_only = set_read_only


bp = Blueprint('tsview', __name__,
               template_folder='tsview_templates',
               static_folder='tsview_static',
)


def series_names(tshclass, engine):
    return sorted(
        name
        for name, kind in tshclass().list_series(engine).items()
        if kind == 'primary'
    )


def homeurl():
    homeurl = url_for('tsview.home')
    return homeurl[:homeurl.rindex('/')] + '/'


def tsview(engine, tshclass=timeseries, series_names=series_names):

    @bp.route('/tsview')
    def home():
        return render_template('tsview.html',
                               homeurl=homeurl(),
                               series=request.args.getlist("series"))

    class logargs(_argsdict):
        defaults = {
            'series': None,
            'seriesvocab': lambda: series_names(tshclass, engine)
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
        args = logargs(request.args)
        tsh = tshclass()
        with engine.begin() as cn:
            log = tsh.log(
                cn,
                name=args.series
            )

        if not log:
            return 'No result.'

        return pd.DataFrame(reversed(log)).to_html(index=False)

    @bp.route('/tsdelete')
    def tsdelete():
        return render_template('tsedit.html',
                               edit_kind="Delete",
                               homeurl=homeurl())

    @bp.route('/tsrename')
    def tsrename():
        return render_template('tsedit.html',
                               edit_kind="Rename",
                               homeurl=homeurl())

    @bp.route('/tsformula')
    def tsformula():
        import json
        import tshistory_formula.funcs
        from tshistory_formula.interpreter import jsontypes

        def check_arg(name, typ):
            if name.endswith('list'):
                typ = "List[%s]" % typ
            return (name, typ)

        spec = [
            (op_name, [check_arg(name, typ) for name, typ in op_spec.items()])
            for op_name, op_spec in json.loads(jsontypes()).items()
        ]
        return render_template('tsformula.html',
                               homeurl=homeurl(),
                               spec=json.dumps(spec))

    return bp
