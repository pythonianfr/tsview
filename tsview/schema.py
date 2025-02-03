from pathlib import Path

from sqlhelp import sqlfile
from tshistory.schema import kvapi
from tsview import __version__


SCHEMA = Path(__file__).parent / 'schema.sql'


class tsview_schema:

    def __init__(self, namespace):
        self.namespace = namespace

    def create(self, engine):
        with engine.begin() as cn:
            cn.execute('drop schema if exists "tsview" cascade')
            cn.execute('create schema "tsview"')
            cn.execute(sqlfile(SCHEMA, ns="tsview"))

        kvstore = kvapi.kvstore(str(engine.url), namespace=f'{self.namespace}-kvstore')
        kvstore.set('tsview-version', __version__)
