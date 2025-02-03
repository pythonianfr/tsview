from sqlalchemy import create_engine

from tshistory.migrate import (
    Migrator as _Migrator,
    version
)

from tsview import __version__
from tsview.schema import tsview_schema
from tsview.horizon import Horizon


class Migrator(_Migrator):
    _order = 4
    _package_version = __version__
    _package = 'tsview'


@version('tsview', '0.20.0')
def create_schema(engine, namespace, interactive):
    initialize_horizon(engine.url)


def initialize_horizon(db_uri):
    engine = create_engine(db_uri)
    tsview_schema('tsh').create(engine)
    api = Horizon(engine)
    def_1 = {
        'fromdate': '(shifted (today ) #:days -15)',
        'todate': '(shifted (today ) #:days 7)',
        'label': '-15d-+7d',
    }
    def_2 = {
        'fromdate': '(shifted (today ) #:days -93)',
        'todate': '(shifted (today ) #:days 31)',
        'label': '3months',
    }
    def_3 = {
        'fromdate': '(shifted (today ) #:days -366)',
        'todate': '(shifted (today ) #:days 31)',
        'label': '1year',
    }

    api.add(def_1)
    api.add(def_2)
    api.add(def_3)
