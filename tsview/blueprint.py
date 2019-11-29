import json
import pandas as pd
from flask import Blueprint, request, render_template, url_for

from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter
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


def haseditor():
    try:
        import tshistory_editor
        return True
    except ImportError:
        return False


def hasformula():
    try:
        import tshistory_formula
        return True
    except ImportError:
        return False


if hasformula():
    import tshistory_formula.funcs
    from tshistory_formula.registry import FUNCS
    from tshistory_formula.interpreter import jsontypes


def homeurl():
    homeurl = url_for('tsview.home')
    return homeurl[:homeurl.rindex('/')] + '/'


PERMISSIONS = ('catalog', 'viewseries', 'rename', 'delete')


def tsview(engine,
           tshclass=timeseries,
           series_names=series_names,
           has_permission=lambda perm: True):

    @bp.route('/tsview')
    def home():
        if not has_permission('viewseries'):
            return 'Nothing to see there.'

        return render_template('tsview.html',
                               homeurl=homeurl(),
                               haseditor=json.dumps(haseditor()),
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
        if not has_permission('viewseries'):
            return 'Nothing to see there.'
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
        if has_permission('delete'):
            return render_template('tsedit.html',
                                   edit_kind="Delete",
                                   homeurl=homeurl())

        return 'You do not have the delete capability.'

    @bp.route('/tsrename')
    def tsrename():
        if has_permission('rename'):
            return render_template('tsedit.html',
                                   edit_kind="Rename",
                                   homeurl=homeurl())

        return 'You do not have the rename capability.'

    @bp.route('/tsformula')
    def tsformula():
        if not hasformula():
            return ''
        if not has_permission('viewseries'):
            return 'Nothing to see there.'

        def check_arg(name, typ):
            if name.endswith('list'):
                typ = "List[%s]" % typ
            return (name, typ)

        spec = sorted([
            (op_name, [
                check_arg(name, typ)
                for name, typ in op_spec.items()
            ])
            for op_name, op_spec in json.loads(jsontypes()).items()],
                      key=lambda x: ("\x00",) if x[0] == 'series' else x
        )
        return render_template('tsformula.html',
                               homeurl=homeurl(),
                               spec=json.dumps(spec))

    @bp.route('/tsformula/pygmentize', methods=['POST'])
    def tsformula_pygmentize():
        if not has_permission('viewseries'):
            return 'Nothing to see there.'
        if not hasformula():
            return ''

        return json.dumps(highlight(
            request.data,
            get_lexer_by_name("lisp"),
            HtmlFormatter()
        ))

    @bp.route('/tsformula/operators')
    def formula_operators():
        if not hasformula():
            return ''
        if not has_permission('viewseries'):
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

    return bp
