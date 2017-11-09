from pathlib import Path

import pytest
from pytest_sa_pg import fixture
from tshistory import schema

DATADIR = Path(__file__).parent / 'test/data'

engine = fixture.engine_fixture(schema.meta, DATADIR, 5433,
                                # tshistory schema builder
                                schema.init)


def pytest_addoption(parser):
    parser.addoption('--refresh-refs', action='store_true', default=False,
                     help='refresh reference outputs')


@pytest.fixture
def refresh(request):
    return request.config.getoption('--refresh-refs')
