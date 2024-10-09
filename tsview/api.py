import pandas as pd
from psyl import lisp

from tsview.moment import _MOMENT_ENV


class Horizon():
    __slots__ = ('engine',)

    def __init__(self, engine):
        self.engine = engine

    def add(self, definition):
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

    def _replace_today(self, formula_bounds, ref_date):
        return formula_bounds.replace('today', f'date "{ref_date}"' )

    def eval_bounds(self, label, ref_date, step=0):
        """
        - label is the user defined name of the horizon
            i.e. "3 weeks", "1 year", etc...
        - ref_date is a date (initialized at today) sent
            by the client (in string format)
        - step is a integer that slides the frame
            - 1 is a slide on the left
            + 1 is a slide on the right
        """
        bounds = self._get_bounds(label)
        from_value_date = lisp.evaluate(
            self._replace_today(
                bounds[0],
                ref_date,
            )
            , env=_MOMENT_ENV
        )
        to_value_date = lisp.evaluate(
            self._replace_today(
                bounds[1],
                ref_date,
            )
            , env=_MOMENT_ENV
        )
        if step != 0:
            interval = to_value_date - from_value_date
            ref_date = pd.Timestamp(ref_date) + interval * step
            ref_date = str(ref_date)
            from_value_date = from_value_date + interval * step
            to_value_date = to_value_date + interval * step
        else:
            # homogenize format
            ref_date=str(pd.Timestamp(ref_date))

        return {
            'fromdate': str(from_value_date),
            'todate': str(to_value_date),
            'ref-date': ref_date
        }
