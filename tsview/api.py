from sqlalchemy import create_engine
import pandas as pd


class Horizon():
    __slots__ = ('engine',)

    def __init__(self, engine):
        self.engine = engine

    def update(self, definition):
        assert isinstance(definition, dict)
        assert set(definition.keys()) == {
            'label', 'fromdate', 'todate'
        }
        with self.engine.begin() as cn:
            max_rank = cn.execute(
                'select rank from tsview.horizon '
                'order by rank desc limit 1'
            ).scalar()
            max_rank = 0 if max_rank is None else max_rank
            labels = cn.execute('select label from tsview.horizon').fetchall()
            if len(labels) and (definition['label'] in [l for (l,) in labels]):
                return
            cn.execute(
                'insert into tsview.horizon '
                    '(label, "fromdate", todate, rank) '
                    'values (%(label)s, %(fromdate)s, %(todate)s, %(rank)s) '
                ,
                label=definition['label'],
                fromdate=definition['fromdate'],
                todate=definition['todate'],
                rank=max_rank + 1
        )

    def get_choices(self):
        with self.engine.begin() as cn:
            result = cn.execute(
                        'select label '
                        'from tsview.horizon '
                        'order by rank asc'
                     )

            return [elt[0] for elt in result.fetchall()]

    def _get_bounds(self, label):
        with self.engine.begin() as cn:
            result = cn.execute(
                        'select fromdate, todate '
                        'from tsview.horizon '
                        'where tsview.horizon.label = %(label)s '
                        , label=label
                     )

            return result.fetchall()[0]

    def get_all(self):
        with self.engine.begin() as cn:
            query = cn.execute(
                'select label, fromdate, todate, rank '
                'from tsview.horizon '
                'order by rank asc')
            colnames = query.keys()
            result = query.fetchall()
            named = [
                {name: value for (name, value) in zip(colnames, row)}
                for row in result
            ]
            return named
