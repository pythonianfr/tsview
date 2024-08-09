import io
import json
from pathlib import Path
import pandas as pd

from tshistory.testutil import (
    genserie,
    gengroup,
    utcdt
)

from tsview.util import format_formula

DATADIR = Path(__file__).parent / 'data'


def test_log(client, tsa):
    series = pd.Series(
        [1, 2, 3],
        index=pd.date_range(
            utcdt(2020, 1, 1),
            freq='d',
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
        'class="nv">add</span>\n'
        '<span class="w">    </span><span class="p">(</span><span class="nv">series</span><span '
        'class="w"> </span><a class="s" href="/tsinfo?name=foo">&quot;foo&quot;</a><span '
        'class="p">)</span>\n'
        '<span class="w">    </span><span class="p">(</span><span class="nv">+</span>\n'
        '<span class="w">        </span><span class="mi">3</span>\n'
        '<span class="w">        </span><span class="p">(</span><span class="nv">series</span><span '
        'class="w"> </span><a class="s" href="/tsinfo?name=bar">&quot;bar&quot;</a><span '
        'class="p">)))</span>\n'
        '</pre></div>\n'
    )

    f = '(integration "foo" "bar")'
    f2 = format_formula(f)
    assert f2 == (
        '<div class="highlight"><pre><span></span><span class="p">(</span><span '
        'class="nv">integration</span>\n'
        '<span class="w">    </span><a class="s" href="/tsinfo?name=foo">&quot;foo&quot;</a>\n'
        '<span class="w">    </span><a class="s" href="/tsinfo?name=bar">&quot;bar&quot;</a><span '
        'class="p">)</span>\n'
        '</pre></div>\n'
    )

    f = '(add (integration "foo" "bar"))'
    f2 = format_formula(f)
    assert f2 == (
        '<div class="highlight"><pre><span></span><span class="p">(</span><span '
        'class="nv">add</span>\n'
        '<span class="w">    </span><span class="p">(</span><span '
        'class="nv">integration</span><span class="w"> </span><a class="s" '
        'href="/tsinfo?name=foo">&quot;foo&quot;</a><span class="w"> </span><a class="s" '
        'href="/tsinfo?name=bar">&quot;bar&quot;</a><span class="p">))</span>\n'
        '</pre></div>\n'
    )


def test_series_metadata_and_formulas(client, tsa):
    for series in tsa.catalog(allsources=False).values():
        for name, _ in series:
            tsa.delete(name)

    series = pd.Series(
        [1, 2, 3],
        index=pd.date_range(
            pd.Timestamp('2022-1-1'),
            freq='d',
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

    tsa.update_metadata('base', {'foo': 'bar'})

    res = client.get('/tssearch/allmetadata')
    assert res.json == {
        'base': {
            'foo': 'bar',
            'index_dtype': '<M8[ns]',
            'index_type': 'datetime64[ns]',
            'supervision_status': 'unsupervised',
            'tablename': 'base',
            'tzaware': False,
            'value_dtype': '<f8',
            'value_type': 'float64'
        },
        'f1': {
            'contenthash': 'ea1aded0c5f2e5e642ac999ecbbee08acfc3ab04',
            'formula': '(series "base")',
            'index_dtype': '<M8[ns]',
            'index_type': 'datetime64[ns]',
            'tzaware': False,
            'value_dtype': '<f8',
            'value_type': 'float64'
        },
        'f2': {
            'contenthash': '036269f058cb14d189a600b827ad8d1ac2a2dfcf',
            'formula': '(* 3.14 (series "base"))',
            'index_dtype': '<M8[ns]',
            'index_type': 'datetime64[ns]',
            'tzaware': False,
            'value_dtype': '<f8',
            'value_type': 'float64'
        }
    }


def test_group_formulas(client, tsa):
    df = gengroup(
        n_scenarios=3,
        from_date=pd.Timestamp('2022-1-1'),
        length=5,
        freq='d',
        seed=2
    )
    tsa.group_replace(
        'base',
        df,
        author='Babar'
    )

    tsa.register_group_formula(
        'gf1',
        '(group-add (group "base") (group "base"))'
    )
    tsa.register_group_formula(
        'f2',
        '(group "gf1"))'
    )

    res = client.get('/groupsearch/allformula')
    assert res.json == {
        'f2': '(group "gf1")',
        'gf1': '(group-add (group "base") (group "base"))'
    }

    # metadata

    series = pd.Series(
        [1, 2, 3],
        index=pd.date_range(
            pd.Timestamp('2022-1-1'),
            freq='d',
            periods=3
        )
    )
    tsa.update('base', series, 'Babar')

    tsa.register_formula(
        'f1',
        '(series "base")'
    )

    tsa.register_formula_bindings(
        'bg',
        'f1',
        pd.DataFrame(
            [
                ['base', 'base', 'group']
            ],
            columns=('series', 'group', 'family')
        )
    )

    res = client.get('/groupsearch/allmetadata')
    out = res.json
    out['base'].pop('tablename')
    assert out == {
        'base': {
            'index_dtype': '<M8[ns]',
            'index_type': 'datetime64[ns]',
            'tzaware': False,
            'value_dtype': '<f8',
            'value_type': 'float64'
        },
        'bg': {
            'bindings': '[{"series":"base","group":"base","family":"group"}]',
            'boundseries': 'f1',
            'index_dtype': '<M8[ns]',
            'index_type': 'datetime64[ns]',
            'tzaware': False,
            'value_dtype': '<f8',
            'value_type': 'float64'
        },
        'f2': {
            'formula': '(group "gf1")',
            'index_dtype': '<M8[ns]',
            'index_type': 'datetime64[ns]',
            'tzaware': False,
            'value_dtype': '<f8',
            'value_type': 'float64'},
        'gf1': {
            'formula': '(group-add (group "base") (group "base"))',
            'index_dtype': '<M8[ns]',
            'index_type': 'datetime64[ns]',
            'tzaware': False,
            'value_dtype': '<f8',
            'value_type': 'float64'
        }
    }


def test_spec_types(client):
    res = client.get('/queryspec')
    assert sorted(res.json) == [
        ['<',
         [['return', 'query'], ['key', 'str'],['value', 'Union[str, Number, bool]']]],
        ['<=',
         [['return', 'query'], ['key', 'str'], ['value', 'Union[str, Number, bool]']]],
        ['=',
         [['return', 'query'], ['key', 'str'], ['value', 'Union[str, Number, bool]']]],
        ['>',
         [['return', 'query'], ['key', 'str'], ['value', 'Union[str, Number, bool]']]],
        ['>=',
         [['return', 'query'], ['key', 'str'], ['value', 'Union[str, Number, bool]']]],
        ['by.and', [['return', 'query'], ['items', 'Packed[query]']]],
        ['by.cache', [['return', 'query']]],
        ['by.cachepolicy', [['return', 'query'], ['query', 'str']]],
        ['by.everything', [['return', 'query']]],
        ['by.formula', [['return', 'query'], ['query', 'str']]],
        ['by.formulacontents', [['return', 'query'], ['query', 'str']]],
        ['by.internal-metaitem',
         [['return', 'query'], ['key', 'str'], ['value', 'Union[str, Number, bool]']]],
        ['by.metaitem',
         [['return', 'query'], ['key', 'str'], ['value', 'Union[str, Number, bool]']]],
        ['by.metakey', [['return', 'query'], ['key', 'str']]],
        ['by.name', [['return', 'query'], ['query', 'str']]],
        ['by.not', [['return', 'query'], ['item', 'query']]],
        ['by.or', [['return', 'query'], ['items', 'Packed[query]']]],
        ['by.source', [['return', 'query'], ['source', 'str']]],
        ['by.tzaware', [['return', 'query']]]
    ]


def test_formula_form_base(engine, client, tsa):
    with engine.begin() as cn:
        cn.execute('delete from tsh.registry')

    ts = genserie('2024-1-1', 'd', 3)
    tsa.update('crude-a', ts, 'Babar')

    user_file = DATADIR / 'goodformula.csv'
    # the user is pushing its own formulas

    response = client.put(
        '/updateformulas',
        upload_files=[
            ('new_formula.csv',
             user_file.name,
             user_file.read_bytes(),
             'text/csv')
        ]
    )
    assert response.status_code == 200
    assert response.json == {
        'crash': '',
        'errors': {'missing': ['crude-b', 'crude-c', 'gas-a', 'gas-b', 'gas-c']},
        'output': {}, 'status': 'invalid', 'warnings': {}
    }


    # really do it
    for name in ('crude-b', 'crude-b', 'crude-c', 'gas-a', 'gas-b', 'gas-c'):
        tsa.update(name, ts, 'Babar')

    _posted = client.put(
        '/updateformulas',
        {'reallydoit': True},
        upload_files=[
            ('new_formula.csv',
             user_file.name,
             user_file.read_bytes(),
             'text/csv')
        ]
    )
    # the user is downloading the current formulaes
    response = client.get('/downloadformulas')
    formula_inserted = pd.read_csv(user_file)
    formula_downloaded = pd.read_csv(io.StringIO(response.text))
    assert set(formula_inserted['text']) == set(formula_downloaded['text'])

    assert tsa.internal_metadata('arith2')['tzaware'] is False

    # We reinsert the donwloaded formulaes and check that everything is kept in the process
    response = client.put(
        '/updateformulas',
        {'reallydoit': True},
        upload_files=[
            ('new_formula.csv',
             'formulareinserted.csv',
             formula_downloaded.to_csv().encode(),
             'text/csv')
        ]
    )

    # confirmation
    response = client.get('/downloadformulas')

    # finaly
    formula_roundtripped = pd.read_csv(io.StringIO(response.text))
    assert formula_roundtripped.equals(formula_downloaded)

    # bogus formulas
    user_file = DATADIR / 'badformula.csv'
    formula_inserted = pd.read_csv(user_file)
    # the user is pushing its own formulaes
    response = client.put(
        '/updateformulas',
        upload_files=[
            ('new_formula.csv',
             user_file.name,
             user_file.read_bytes(),
             'text/csv')
        ]
    )
    assert response.json == {
        'crash': '',
        'errors': {
            'syntax': [
                'syntax',
                "syntax_keyword : BadKeyword('keyword `#:ffill` not followed by a value')",
                'timezone : ValueError("Formula `constant` has tzaware vs tznaive series:`(\'gas-b\', (\'add, \'series)):tznaive`,`(\'constant\', (\'add, \'constant)):tzaware`")'
            ]
        },
        'output': {},
        'status': 'invalid',
        'warnings': {'existing': ['prio1']}
    }
