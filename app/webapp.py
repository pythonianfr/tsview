from sqlalchemy import create_engine
import pandas as pd
from flask import Flask, request, render_template

from inireader import reader

from tshistory.tsio import TimeSerie

from tsview.util import argsdict as _argsdict
from tsview.plot import plot


app = Flask(__name__)


def serie_names(engine):
    sql = 'select name from registry order by name'
    return [name for name, in engine.execute(sql).fetchall()]


def author_names(engine):
    sql = 'select distinct author from changeset order by author'
    return [name for name, in engine.execute(sql).fetchall()]


def maxrev(engine):
    sql = 'select max(id) from changeset'
    return engine.execute(sql).scalar()


def startapp(engine):

    class viewargs(_argsdict):
        defaults = {
            'outputtype': 'plot',
            'outputtypevocab': ('plot', 'table'),
            'series': (),
            'seriesvocab': serie_names(engine)
        }
        types = {
            'series': list
        }


    @app.route('/tsview')
    def home():
        args = viewargs(request.args)
        return render_template('tsview.html', **args)


    @app.route('/tsplot')
    def tsplot():
        args = viewargs(request.args)
        return plot(args, engine)


    class logargs(_argsdict):
        defaults = {
            'limit': 20,
            'series': (),
            'seriesvocab': serie_names(engine),
            'authors': (),
            'authorsvocab': author_names(engine),
            'fromrev': 0,
            'torev': maxrev(engine)
        }
        types = {
            'series': list,
            'authors': list,
            'limit': int,
            'fromrev': int,
            'torev': int
        }


    @app.route('/tsviewlog')
    def tsviewlog():
        args = logargs(request.args)
        return render_template('tslog.html', **args)


    @app.route('/tslog')
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


def kickoff(host, port, dburi):
    engine = create_engine(dburi)
    startapp(engine)
    app.run(host=host, port=port, threaded=True)


if __name__ == '__main__':
    import socket
    ipaddr = socket.gethostbyname(socket.gethostname())
    config = reader('tsview.ini')
    kickoff(ipaddr, 5000, config['db']['uri'])
