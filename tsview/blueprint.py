import pandas as pd
from flask import Blueprint, request, render_template, url_for

from dash import _utils

from tshistory.tsio import timeseries

from tsview.util import argsdict as _argsdict
from tsview.plot import plot


# monkeypatch dash utility
def set_read_only(self, names, msg='Attribute is read-only'):
    return

_utils.AttributeDict.set_read_only = set_read_only


bp = Blueprint('tsview', __name__,
               template_folder='tsview_templates',
               static_folder='tsview_static',
)


def serie_names(engine):
    sql = 'select seriename from tsh.registry order by seriename'
    return [name for name, in engine.execute(sql).fetchall()]


def author_names(engine):
    sql = 'select distinct author from tsh.changeset order by author'
    return [name for name, in engine.execute(sql).fetchall()]


def maxrev(engine):
    sql = 'select max(id) from tsh.changeset'
    return engine.execute(sql).scalar()


def homeurl():
    homeurl = url_for('tsview.home')
    return homeurl[:homeurl.rindex('/')] + '/'


def tsview(engine, tshclass=timeseries, serie_names=serie_names):

    @bp.route('/tsview')
    def home():
        return render_template('tsview.html',
                               homeurl=homeurl(),
                               series=request.args.getlist("series"))

    class logargs(_argsdict):
        defaults = {
            'limit': 20,
            'series': (),
            'seriesvocab': lambda: serie_names(engine),
            'authors': (),
            'authorsvocab': lambda: author_names(engine),
            'fromrev': 0,
            'torev': lambda: maxrev(engine)
        }
        types = {
            'series': list,
            'authors': list,
            'limit': int,
            'fromrev': int,
            'torev': int
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
            log = tsh.log(cn, limit=args.limit, names=args.series,
                          authors=set(args.authors),
                          fromrev=args.fromrev, torev=args.torev)

        if not log:
            return 'No result.'

        return pd.DataFrame(log).to_html(index=False)

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

    return bp
