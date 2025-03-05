from pathlib import Path

from sqlhelp import sqlfile
from tshistory.schema import kvapi

from tsview import __version__
from tsview.horizon import Horizon


SCHEMA = Path(__file__).parent / 'schema.sql'


class tsview_schema:

    def __init__(self, namespace):
        self.namespace = namespace

    def create(self, engine):
        with engine.begin() as cn:
            cn.execute('drop schema if exists "tsview" cascade')
            cn.execute('create schema "tsview"')
            cn.execute(sqlfile(SCHEMA, ns="tsview"))

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

        kvstore = kvapi.kvstore(str(engine.url), namespace=f'{self.namespace}-kvstore')
        kvstore.set('tsview-version', __version__)
