from flask import Flask

from sqlalchemy import create_engine
from tshistory_rest.blueprint import blueprint as rest_blueprint

from tsview.blueprint import tsview
from tsview.dashboard import historic


app = Flask('tsview')


def kickoff(host, port, dburi, debug=False):
    engine = create_engine(dburi)
    app.register_blueprint(
        rest_blueprint(engine),
        url_prefix='/api'
    )
    app.register_blueprint(tsview(engine))
    historic(app, engine)
    app.run(host=host, port=port, debug=debug, threaded=not debug)
