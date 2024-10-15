from pathlib import Path

from sqlhelp import sqlfile


SCHEMA = Path(__file__).parent / 'schema.sql'


class TsviewSchema():

    def create(self, engine, reset=False):
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
