import pytest

from tsview.api import Horizon


def remove_metadata(tsrepr):
    if 'Freq' in tsrepr or 'Name' in tsrepr:
        return tsrepr[:tsrepr.rindex('\n')]
    return tsrepr


def assert_df(expected, df):
    exp = remove_metadata(expected.strip())
    got = remove_metadata(df.to_string().strip())
    assert exp == got


def test_horizon_roundtrip(engine):
    api = Horizon(engine)
    def_1 = {
        'fromdate': '(shifted (today ) #:days -15)',
        'todate': '(shifted (today ) #:days 7)',
        'label': '-15d-+7d',
    }
    def_2 = {
        'fromdate': '(shifted (today ) #:days -93)',
        'todate': '(shifted (today ) #:days 31)',
        'label': '3 months',
    }
    def_3 = {
        'fromdate': '(shifted (today ) #:days -366)',
        'todate': '(shifted (today ) #:days 31)',
        'label': '1 year',
    }

    api.add(def_1)
    api.add(def_2)
    api.add(def_3)

    all = api.get_all()
    assert all == [
        {'fromdate': '(shifted (today ) #:days -15)',
         'id': 1,
         'label': '-15d-+7d',
         'rank': 1,
         'todate': '(shifted (today ) #:days 7)'},
        {'fromdate': '(shifted (today ) #:days -93)',
         'id': 2,
         'label': '3 months',
         'rank': 2,
         'todate': '(shifted (today ) #:days 31)'},
        {'fromdate': '(shifted (today ) #:days -366)',
         'id': 3,
         'label': '1 year',
         'rank': 3,
         'todate': '(shifted (today ) #:days 31)'}
    ]

    choices = api.get_choices()
    assert choices == [
        '-15d-+7d',
        '3 months',
        '1 year',
    ]

    bounds = api._get_bounds('3 months')
    assert bounds == (
            '(shifted (today ) #:days -93)',
            '(shifted (today ) #:days 31)'
        )

    # the client will alter the definition

    from_client = [
        {'fromdate': '(shifted (today ) #:days -15)',
         'id': 1,
         'label': '-15d- <> +7d',   # <- change
         'todate': '(shifted (today ) #:days 7)',
         'action': 'update'},
        {'fromdate': '(shifted (today ) #:days -93)',
         'id':2,
         'label': '3 months',       # <- deletion
         'todate': '(shifted (today ) #:days 31)',
         'action': 'delete' },
        {'fromdate': '(shifted (today ) #:days -366)',
         'id': 3,
         'label': '1 year',
         'todate': '(shifted (today ) #:days 31)',
         'action': 'update'},
        {'fromdate': '(shifted (today ) #:days -1000)',
         'id': -1,
         'label': '3 year',         # <- creation
         'todate': '(shifted (today ) #:days 31)',
         'action': 'create'}
    ]

    api.replace_all(from_client)

    all = api.get_all()

    assert all == [
        {'fromdate': '(shifted (today ) #:days -15)',
         'id': 1,
         'label': '-15d- <> +7d',
         'rank': 1,
         'todate': '(shifted (today ) #:days 7)'},
        {'fromdate': '(shifted (today ) #:days -366)',
         'id': 3,
         'label': '1 year',
         'rank': 2,
         'todate': '(shifted (today ) #:days 31)'},
        {'fromdate': '(shifted (today ) #:days -1000)',
         'id': 4,
         'label': '3 year',
         'rank': 3,
         'todate': '(shifted (today ) #:days 31)'}
    ]


def test_horizon_evaluate(engine):
    # we insert the same definition as before
    api = Horizon(engine)
    def_1 = {
        'fromdate': '(shifted (today ) #:days -15)',
        'todate': '(shifted (today ) #:days 7)',
        'label': '-15d-+7d',
    }
    def_2 = {
        'fromdate': '(shifted (today ) #:days -93)',
        'todate': '(shifted (today ) #:days 31)',
        'label': '3 months',
    }
    def_3 = {
        'fromdate': '(shifted (today ) #:days -366)',
        'todate': '(shifted (today ) #:days 31)',
        'label': '1 year',
    }

    api.add(def_1)
    api.add(def_2)
    api.add(def_3)

    today = '2024-09-17'

    # start
    result = api.eval_bounds('-15d-+7d', today)
    assert result == {
        'fromdate': '2024-09-02 00:00:00',
        'todate': '2024-09-24 00:00:00',
        'ref-date': '2024-09-17 00:00:00'
    }
    ref_date = result['ref-date']

    # navigation of two steps on the past:
    result = api.eval_bounds('-15d-+7d', ref_date, step=-2)
    assert result == {
        'fromdate': '2024-07-20 00:00:00',
        'todate': '2024-08-11 00:00:00',
        'ref-date': '2024-08-04 00:00:00'
    }
    ref_date = result['ref-date']

    # change horizon choice (keep the focus on ref_date)
    result = api.eval_bounds('3 months', ref_date)
    assert result == {
        'fromdate': '2024-05-03 00:00:00',
        'todate': '2024-09-04 00:00:00',
        'ref-date': '2024-08-04 00:00:00'
    }
    ref_date = result['ref-date']

    # one step to the future:
    result = api.eval_bounds('3 months', ref_date, step=1)
    assert result == {
        'fromdate': '2024-09-04 00:00:00',
        'todate': '2025-01-06 00:00:00',
        'ref-date': '2024-12-06 00:00:00'
    }
    ref_date = result['ref-date']
    assert ref_date == '2024-12-06 00:00:00'


def test_translation(engine):
    today = '2024-09-17'
    api = Horizon(engine)
    def_1 = {
        'fromdate': '(shifted (today ) #:days -15)',
        'todate': '(shifted (today ) #:days 7)',
        'label': 'add-15d-+7d',
    }
    api.add(def_1)
    result = api.eval_bounds('add-15d-+7d', today)
    assert result == {
        'fromdate': '2024-09-02 00:00:00',
        'todate': '2024-09-24 00:00:00',
        'ref-date': '2024-09-17 00:00:00'
    }

    translated = api.translate(result, -2)

    assert translated == {
        'fromdate': '2024-07-20 00:00:00',
        'todate': '2024-08-11 00:00:00',
        'ref-date': '2024-08-04 00:00:00'
    }


def test_bad_horizon(engine):
    api = Horizon(engine)
    def_1 = {
        'fromdate': '(shifted (today ) #:days -15)',
        'todate': '(shifted (today ) #:days 7)',
        'label': '-15d-+7d',
    }
    # we introduce a pair of typo
    def_2 = {
        'fromdate': '(shifted (todo ) #:days -93)',
        'todate': '(shifted (today ) #:days 31',
        'label': '3 months',
    }

    batch = [def_1, def_2]
    with pytest.raises(Exception):
        api.replace_all(batch)

    # we permut from and to
    def_2 = {
        'fromdate': '(shifted (today ) #:days 31)',
        'todate': '(shifted (today ) #:days -93)',
        'label': '3 months',
    }
    with pytest.raises(Exception):
        api.replace_all([def_1, def_2])
