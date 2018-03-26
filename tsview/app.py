from flask import Flask

from sqlalchemy import create_engine

from tsview.blueprint import tsview
from tsview.dashboard import historic


app = Flask('tsview')


def kickoff(host, port, dburi):
    engine = create_engine(dburi)
    app.register_blueprint(tsview(engine))
    historic(app, engine)
    app.run(host=host, port=port, threaded=False)
