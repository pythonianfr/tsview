from flask import Flask

from tshistory.http.server import httpapi

from tsview.blueprint import tsview
from tsview.history import historic


def make_app(tsa):
    app = Flask('tsview')
    app.register_blueprint(
        httpapi(tsa).bp,
        url_prefix='/api'
    )
    app.register_blueprint(
        tsview(
            tsa,
            has_permission=lambda perm: True
        )
    )
    historic(app, tsa)
    return app


def kickoff(host, port, tsa, debug=False):
    app = make_app(tsa)
    app.run(host=host, port=port, debug=debug, threaded=not debug)
