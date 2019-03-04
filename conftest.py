from pathlib import Path

import pytest
from pytest_sa_pg import db
from sqlalchemy import create_engine
from tshistory import schema

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
    sch.destroy(e)
    schema.init_schemas(e)
    return e


def pytest_addoption(parser):
    parser.addoption('--refresh-refs', action='store_true', default=False,
                     help='refresh reference outputs')


@pytest.fixture
def refresh(request):
    return request.config.getoption('--refresh-refs')
