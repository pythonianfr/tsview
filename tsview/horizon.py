import pandas as pd

from tsview.moment import (
    eval_moment,
    ConfigurationError,
)


class Horizon():
    __slots__ = ('engine',)

    def __init__(self, engine):
        self.engine = engine

    def add(self, definition: dict):
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
            if len(labels) and (definition['label'] in [lab for (lab,) in labels]):
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

    def _delete(self, cn, batch: list):
        if not len(batch):
            return
        sql = (
            'delete '
            'from tsview.horizon '
            'where tsview.horizon.id = %(id)s'
        )
        cn.execute(sql, batch)

    def _create(self, cn, batch: list):
        if not len(batch):
            return
        sql = (
            'insert into tsview.horizon '
            '(label, fromdate, todate, rank) '
            'values (%(label)s, %(fromdate)s, %(todate)s, %(rank)s)'
        )
        cn.execute(sql, batch)

    def _update(self, cn, batch: list):
        if not len(batch):
            return
        sql = (
            'update tsview.horizon '
            'set rank = %(rank)s ,'
            '    fromdate = %(fromdate)s,'
            '    todate = %(todate)s,'
            '    label = %(label)s '
            'where tsview.horizon.id = %(id)s'
        )
        cn.execute(sql,batch)

    def replace_all(self, batch: list):
        """
        dispatch definitions to corresponding methods
        The orders of the operation (delete, update, create)
        are important to respect the rank unicity constraint
        """
        self._validate_config(batch)

        to_delete = [
            spec
            for spec in batch
            if spec['action'] == 'delete'
        ]
        others = [
            spec
            for spec in batch
            if spec['action'] != 'delete'
        ]
        # attribute rank:
        for rank, spec in enumerate(others, start=1):
            spec['rank'] = rank

        to_update = [
            spec
            for spec in others
            if spec['action'] == 'update'
        ]
        to_create = [
            spec
            for spec in others
            if spec['action'] == 'create'
        ]
        with self.engine.connect() as cn:
            self._delete(cn, to_delete)
            self._update(cn, to_update)
            self._create(cn, to_create)

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
                'where tsview.horizon.label = %(label)s ',
                label=label
            )

            return result.fetchall()[0]

    def get_all(self):
        with self.engine.begin() as cn:
            query = cn.execute(
                'select id, label, fromdate, todate, rank '
                'from tsview.horizon '
                'order by rank asc'
            )
            colnames = query.keys()
            result = query.fetchall()
            named = [
                {
                    name: value
                    for (name, value) in zip(colnames, row)
                }
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
        from_value_date = eval_moment(
            self._replace_today(
                bounds[0],
                ref_date,
            )
        )
        to_value_date = eval_moment(
            self._replace_today(
                bounds[1],
                ref_date,
            )
        )
        if step != 0:
            interval = to_value_date - from_value_date
            ref_date = pd.Timestamp(ref_date) + interval * step
            ref_date = str(ref_date)
            from_value_date = from_value_date + interval * step
            to_value_date = to_value_date + interval * step
        else:
            # homogenize format
            ref_date = str(pd.Timestamp(ref_date))

        return {
            'fromdate': str(from_value_date),
            'todate': str(to_value_date),
            'ref-date': ref_date
        }

    def translate(self, bounds, step):
        assert set(bounds.keys()) == {
            'fromdate', 'todate', 'ref-date'
        }
        from_value_date = pd.Timestamp(bounds['fromdate'])
        to_value_date = pd.Timestamp(bounds['todate'])
        ref_date = pd.Timestamp(bounds['ref-date'])
        delta = to_value_date - from_value_date

        return {
            'fromdate': str(from_value_date + delta * step),
            'todate': str(to_value_date + delta * step),
            'ref-date': str(ref_date + delta * step)
        }

    def _validate_config(self, batch):
        # labels must be unique
        labels = [elt['label'] for elt in batch]
        if len(labels) != len(set(labels)):
            raise ConfigurationError(
                'Labels must be unique'
            )
        for row in batch:
            # all keys must be presents
            assert set(row.keys()) == {
                'label', 'fromdate', 'todate', 'action', 'id',
            }
            # bounds must be evaluable
            from_value_date = eval_moment(
                row['fromdate'],
            )
            to_value_date = eval_moment(
                row['todate'],
            )
            # to_value_date must be after from_value_date
            if pd.Timestamp(to_value_date) <= pd.Timestamp(from_value_date):
                raise ConfigurationError(
                    '"From" must precede "To"'
                )
