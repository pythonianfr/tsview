import pandas as pd
from flask import Blueprint, request, render_template

from tshistory.tsio import TimeSerie

from tsview.util import argsdict as _argsdict
from tsview.plot import plot


bp = Blueprint('tsview', __name__,
               template_folder='tsview_templates',
               static_folder='tsview_static',
)


def serie_names(engine):
    sql = 'select name from tsh.registry order by name'
    return [name for name, in engine.execute(sql).fetchall()]


def author_names(engine):
    sql = 'select distinct author from tsh.changeset order by author'
    return [name for name, in engine.execute(sql).fetchall()]


def maxrev(engine):
    sql = 'select max(id) from tsh.changeset'
    return engine.execute(sql).scalar()


def tsview(engine):

    class viewargs(_argsdict):
        defaults = {
            'outputtype': 'plot',
            'outputtypevocab': ('plot', 'table'),
            'series': (),
            'seriesvocab': lambda: serie_names(engine)
        }
        types = {
            'series': list
        }

    @bp.route('/tsview')
    def home():
        args = viewargs(request.args)
        return render_template('tsview.html', **args)

    @bp.route('/tsplot')
    def tsplot():
        args = viewargs(request.args)
        return plot(args, engine)

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
        tsh = TimeSerie()
        with engine.connect() as cn:
            log = tsh.log(cn, limit=args.limit, names=args.series,
                          authors=set(args.authors), diff=args.diff,
                          fromrev=args.fromrev, torev=args.torev)

        if not log:
            return 'No result.'

        return pd.DataFrame(log).to_html(index=False)

    return bp
