from flask import Flask

from sqlalchemy import create_engine
from tshistory.api import timeseries
from tshistory_rest.blueprint import blueprint as rest_blueprint

from tsview.blueprint import tsview
from tsview.history import historic


def make_app(tsa):
    app = Flask('tsview')
    app.register_blueprint(
        rest_blueprint(tsa),
        url_prefix='/api'
    )
    app.register_blueprint(
        tsview(
            tsa.engine,
            has_permission=lambda perm: True
        )
    )
    historic(app, tsa)
    return app


def kickoff(host, port, dburi, handler, debug=False):
    engine = create_engine(dburi)
    tsa =  timeseries(
        dburi,
        handler=handler
    )
    app = make_app(tsa)
    app.run(host=host, port=port, debug=debug, threaded=not debug)
