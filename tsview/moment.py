import calendar as cal
from datetime import datetime as dt
from psyl import lisp

from dateutil.parser import (
    isoparse,
    parse as defaultparse,
)
from dateutil.relativedelta import relativedelta


class ConfigurationError(Exception):
    pass


def eval_moment(expr):
    try:
        return lisp.evaluate(
            expr,
            env=_MOMENT_ENV
        )
    except LookupError:
        import traceback as tb
        tb.print_exc()
        raise ConfigurationError(
            f'Value {repr(expr)} is not a valid moment expression'
        )


def _parsedatetime(strdt):
    try:
        return isoparse(strdt)
    except ValueError:
        return defaultparse(strdt)


def _last_day_of_month(dt):
    return cal.monthrange(dt.year, dt.month)[1]


def dateshift(
        dt,
        years=0,
        months=0,
        weeks=0,
        days=0,
        hours=0,
        minutes=0,
        seconds=0,
):
    return dt + relativedelta(
        years=years,
        months=months,
        weeks=weeks,
        days=days,
        hours=hours,
        minutes=minutes,
        seconds=seconds,
    )


_MOMENT_ENV = lisp.Env({
    'date': lambda strdate: _parsedatetime(strdate),
    'today': lambda: dt.now().date(),
    'monthstart': lambda dt: dt.replace(day=1),
    'monthend': lambda dt: dt.replace(day=_last_day_of_month(dt)),
    'yearstart': lambda dt: dt.replace(day=1, month=1),
    'yearend': lambda dt: dt.replace(day=31, month=12),
    'shifted': dateshift,
})

