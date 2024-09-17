from tsview.api import Horizon


def remove_metadata(tsrepr):
    if 'Freq' in tsrepr or 'Name' in tsrepr:
        return tsrepr[:tsrepr.rindex('\n')]
    return tsrepr


def assert_df(expected, df):
    exp = remove_metadata(expected.strip())
    got = remove_metadata(df.to_string().strip())
    assert exp == got


def test_horizon_crude(engine):
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

    api.update(def_1)
    api.update(def_2)
    api.update(def_3)

    all = api.get_all()
    assert all == [
        {'fromdate': '(shifted (today ) #:days -15)',
         'label': '-15d-+7d',
         'rank': 1,
         'todate': '(shifted (today ) #:days 7)'},
        {'fromdate': '(shifted (today ) #:days -93)',
         'label': '3 months',
         'rank': 2,
         'todate': '(shifted (today ) #:days 31)'},
        {'fromdate': '(shifted (today ) #:days -366)',
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

