import pandas as pd

from tshistory.testutil import utcdt

from tsview.util import format_formula


def test_log(client, tsa):
    series = pd.Series(
        [1, 2, 3],
        index=pd.date_range(
            utcdt(2020, 1, 1),
            freq='D',
            periods=3
        )
    )
    for d in range(3):
        tsa.update(
            'test-log',
            series,
            'Babar',
            insertion_date=utcdt(2020, 1, 1+d),
            metadata={'comment': f'day {d+1}'}
        )
        series[d] = 42

    res = client.get('/api/series/log?name=test-log')
    assert res.json == [
        {'author': 'Babar',
         'date': '2020-01-01T00:00:00+00:00',
         'meta': {'comment': 'day 1'},
         'rev': 1},
        {'author': 'Babar',
         'date': '2020-01-02T00:00:00+00:00',
         'meta': {'comment': 'day 2'},
         'rev': 2},
        {'author': 'Babar',
         'date': '2020-01-03T00:00:00+00:00',
         'meta': {'comment': 'day 3'},
         'rev': 3}
    ]


def test_formula_formatter():
    f = '(add (series "foo") (+ 3 (series "bar")))'
    f2 = format_formula(f)
    assert f2 == (
        '<div class="highlight"><pre><span></span><span class="p">(</span><span '
        'class="nv">add</span><span class="w"></span>\n'
        '<span class="w">'
        '    </span><span class="p">(</span><span class="nv">series</span><span class="w">'
        ' </span><span class="s">&quot;foo&quot;</span><span class="p">)</span><span class="w">'
        '</span>\n<span class="w">'
        '    </span><span class="p">(</span><span class="nb">+</span><span class="w"></span>\n'
        '<span class="w">        </span><span class="mi">3</span><span class="w"></span>\n'
        '<span class="w">        </span><span class="p">'
        '(</span><span class="nv">series</span><span class="w">'
        ' </span><span class="s">&quot;bar&quot;</span><span class="p">)))</span>'
        '<span class="w"></span>\n</pre></div>\n'
    )


def test_series_formulas(client, tsa):
    series = pd.Series(
        [1, 2, 3],
        index=pd.date_range(
            utcdt(2020, 1, 1),
            freq='D',
            periods=3
        )
    )
    tsa.update('base', series, 'Babar')

    tsa.register_formula(
        'f1',
        '(series "base")'
    )
    tsa.register_formula(
        'f2',
        '(* 3.14 (series "base"))'
    )

    res = client.get('/tssearch/allformula')
    assert res.json == {
        'f1': '(series "base")',
        'f2': '(* 3.14 (series "base"))'
    }

