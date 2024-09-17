from pathlib import Path

from sqlhelp import sqlfile


SCHEMA = Path(__file__).parent / 'schema.sql'


class TsviewSchema():

    def create(self, engine, reset=False):
        with engine.begin() as cn:
            if reset:
                cn.execute(
                    f'drop schema if exists "tsview" cascade'
                )

            hasschema = cn.execute(
                f"select exists (select 1 "
                f"  from information_schema.schemata "
                f"  where schema_name='tsview' "
                f")"
            ).scalar()
            if not hasschema:
                cn.execute(f'create schema "tsview"')
                cn.execute(sqlfile(SCHEMA, ns="tsview"))
