from pathlib import Path

from sqlhelp.pgapi import pgdb as create_engine
from sqlhelp.pgapi import pgdb

from tshistory.migrate import (
    do_fix_indexes,
    Migrator as _Migrator,
    version
)
from tshistory.sqlparser import parse_indexes

from tsview import __version__
from tsview.schema import tsview_schema
from tsview.horizon import Horizon


class Migrator(_Migrator):
    _order = 4
    _package_version = __version__
    _package = 'tsview'


@version('tsview', '0.22.0')
def migrate_fix_tsview_indexes(engine: pgdb, namespace: str, interactive: bool) -> None:
    """Fix tsview-specific indexes to use explicit names"""
    tsview_path = Path(__file__).parent
    tsview_indexes = parse_indexes(
        [tsview_path / 'schema.sql'],
        'tsview'
    )
    do_fix_indexes(engine, 'tsview', interactive, tsview_indexes)


@version('tsview', '0.20.0')
def create_schema(engine: pgdb, namespace: str, interactive: bool) -> None:
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
