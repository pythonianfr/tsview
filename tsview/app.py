from flask import Flask

from sqlalchemy import create_engine

from tsview.blueprint import tsview

app = Flask('tsview')


def kickoff(host, port, dburi):
    engine = create_engine(dburi)
    app.register_blueprint(tsview(engine))
    app.run(host=host, port=port, threaded=True)
