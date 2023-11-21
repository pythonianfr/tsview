import pandas as pd
import dash.html as html

from psyl.lisp import parse

from tshistory_formula.interpreter import Interpreter
from tshistory_formula.helper import inject_toplevel_bindings


MAX_LENGTH = 15


class fancypresenter:
    __slots__ = ('tsa', 'name', 'infos', 'formula')

    def __init__(self, tsa, seriesname, getargs):
        assert tsa.exists(seriesname)
        formula = tsa.formula(seriesname)
        tree = parse(formula)

        def get(name, expr):
            ts = tsa.get(name, **getargs)
            if ts is None:
                # attempt immediate expression interpretation
                # (autotrophic operator)
                i = Interpreter(tsa.engine, tsa.tsh, getargs)
                ts = i.evaluate(
                    inject_toplevel_bindings(
                        expr,
                        getargs
                    )
                )
                if ts is None:
                    return pd.Series(name=name, dtype='float64')
                ts.name = name
                return ts
            return ts

        self.infos = [
            {
                'name': name,
                'ts':  get(name, expr),
                'type': tsa.type(name) or 'autotrophic'
            }
            for name, expr in tsa.tsh.find_series(tsa.engine, tree).items()
        ]
        self.infos.insert(0, {
            'name': seriesname,
            'ts': get(seriesname, tree),
            'type': 'formula'
        })


def build_url(base_url, name, fromdate, todate, author):
    url = base_url + '?name=%s' % name
    if fromdate:
        url = url + '&startdate=%s' % fromdate
    if todate:
        url = url + '&enddate=%s' % todate
    if author:
        url = url + '&author=%s' % author
    return url


def short_div(content):
    if len(content) > MAX_LENGTH:
        shortcontent = content[:MAX_LENGTH] + '(…)'
        return html.Div(
            shortcontent,
            title=content,
            style={'font-size':'small'}
        )
    else:
        return html.Div(
            content,
            style={'font-size':'small'}
        )


def build_div_header(engine, info, href, more_info=None):
    add = [
        html.Div(
            info.get(key, '-'),
            style={'font-size':'small'}
        )
        for key in ['type']
    ]
    name = [
        html.A(
            href=href,
            children=info['name'],
            target="_blank",
            style={'font-size':'small', 'word-wrap': 'break-word'}
        )
    ]
    header = name + add
    if more_info is not None:
        info_metadata = more_info(engine, info['name'])
        if info_metadata:
            metadata = [
                short_div(info.lower().capitalize())
                for _, info in info_metadata.items()
            ]
            header = metadata + header
    return html.Div(header)


def components_table(tsa, id_serie,
                     fromdate=None, todate=None,
                     author=None, additionnal_info=None,
                     base_url=''):
    " function used as callback for tseditor to handle formula "
    kind = tsa.type(id_serie)
    if kind != 'formula':
        return None

    presenter = fancypresenter(
        tsa, id_serie, {
            'from_value_date': fromdate,
            'to_value_date': todate
        }
    )
    infos = presenter.infos

    # collect base series
    df = infos[0]['ts'].to_frame()
    tz = df.index.tz
    for info in infos[1:]:
        ts = info['ts']
        if len(ts) and ts.index.tz != tz:
            ts.index = ts.index.tz_localize(tz)
        df = df.join(ts, how='outer')

    header_css = {
        'max-width': f'{MAX_LENGTH + 3}em',
        'min-width': f'{MAX_LENGTH + 3}em',
        'width': f'{MAX_LENGTH + 3}em',
        'position': 'sticky',
        'top': '0',
        'background-color': 'white'
    }
    corner_css = {
        'left': '0',
        'top':'0',
        'background-color': 'white'
    }
    dates_css = {
        'position': 'sticky',
        'left': '0',
        'background-color': 'white'
    }

    corner = html.Th('', style=corner_css)
    header = html.Tr([corner] + [
        html.Th(
            build_div_header(
                tsa.engine,
                info,
                build_url(
                    base_url, info['name'],
                    fromdate, todate, author
                ),
                additionnal_info
            ),
            style=header_css
        )
        for info in infos
    ])

    table = [header]
    for i in range(len(df)):
        new_line = [
            html.Th(
                df.index[i],
                style=dates_css
            )
        ]
        for info in infos:
            name = info['name']
            new_line.append(
                html.Td(
                    df.iloc[i][name]
                )
            )
        table.append(html.Tr(new_line))
    return html.Table(table)
