from sqlalchemy import create_engine

from flask import Flask, request, render_template

from inireader import reader

from tsview.util import argsdict as _argsdict
from tsview.plot import plot


CONFIG = reader('tsview.ini')
ENGINE = create_engine(CONFIG['db']['uri'])

app = Flask(__name__)


def serie_names(engine):
    sql = 'select name from registry order by name'
    return [name for name, in engine.execute(sql).fetchall()]


class argsdict(_argsdict):
    defaults = {
        'outputtype': 'plot',
        'outputtypevocab': ('plot', 'table'),
        'series': (),
        'seriesvocab': serie_names(ENGINE)
    }
    types = {
        'series': list
    }


@app.route('/')
def home():
    args = argsdict(request.args)
    return render_template('tsview.html', **args)


@app.route('/tsplot')
def tsplot():
    args = argsdict(request.args)
    return plot(args, ENGINE)


if __name__ == '__main__':
    import socket
    ipaddr = socket.gethostbyname(socket.gethostname())
    app.run(host=ipaddr, threaded=True)
