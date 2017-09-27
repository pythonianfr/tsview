from flask import Flask

from sqlalchemy import create_engine

from tsview.blueprint import app as bp, complete_blueprint

app = Flask('tsview')


def kickoff(host, port, dburi):
    engine = create_engine(dburi)
    complete_blueprint(engine)
    app.register_blueprint(bp)
    app.run(host=host, port=port, threaded=True)
