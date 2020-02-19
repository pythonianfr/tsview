from pathlib import Path

import pytest
import webtest
from pytest_sa_pg import db
from sqlalchemy import create_engine

from tshistory import api
from tshistory import schema

from tsview import app


DATADIR = Path(__file__).parent / 'test/data'
DBURI = 'postgresql://localhost:5433/postgres'


@pytest.fixture(scope='session')
def engine(request):
    db.setup_local_pg_cluster(request, DATADIR, 5433, {
        'timezone': 'UTC',
        'log_timezone': 'UTC'}
    )
    e = create_engine(DBURI)
    sch = schema.tsschema()
    sch.create(e)
    return e


class WebTester(webtest.TestApp):

    def _check_status(self, status, res):
        try:
            super(WebTester, self)._check_status(status, res)
        except:
            print('ERRORS', res.errors)
            # raise <- default behaviour on 4xx is silly


@pytest.fixture(scope='session')
def tsa(engine):
    return api.timeseries(
        str(engine.url)
    )


@pytest.fixture(scope='session')
def client(tsa):
    wsgi = app.make_app(tsa)
    yield WebTester(wsgi)
