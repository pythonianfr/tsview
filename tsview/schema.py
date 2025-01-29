from pathlib import Path

from sqlhelp import sqlfile
from tshistory.schema import kvapi, tsschema
from tsview import __version__


SCHEMA = Path(__file__).parent / 'schema.sql'


class tsview_schema(tsschema):

    def create(self, engine, reset=False, **kw):
        super().create(engine, reset=reset, **kw)

        with engine.begin() as cn:
            if reset:
                cn.execute(
                    'drop schema if exists "tsview" cascade'
                )

            hasschema = cn.execute(
                "select exists (select 1 "
                "  from information_schema.schemata "
                "  where schema_name='tsview' )"
            ).scalar()
            if not hasschema:
                cn.execute('create schema "tsview"')
                cn.execute(sqlfile(SCHEMA, ns="tsview"))

        kvstore = kvapi.kvstore(str(engine.url), namespace=f'{self.namespace}-kvstore')
        kvstore.set('tsview-version', __version__)
