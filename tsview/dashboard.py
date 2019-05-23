from flask_caching import Cache
import plotly.graph_objs as go

from dash_core_components import Graph, Location, Dropdown, Slider
from dash_html_components import Div, Button, Br
import dash_html_components as html

import dash

import pandas as pd

from tshistory import tsio


COLOR_BEFORE = 'rgb(20, 200, 20)'
COLOR_CURRENT = 'rgb(0, 0, 250)'
COLOR_AFTER = 'rgb(204, 12, 20)'
COLOR_LAST = 'rgb(0, 0, 0)'


def serie_names(engine):
    sql = 'select seriename from tsh.registry order by seriename'
    return [name for name, in engine.execute(sql).fetchall()]


def unpack_dates(graphdata):
    fromdate = None
    todate = None
    if graphdata and 'xaxis.range[0]' in graphdata:
        fromdate = pd.to_datetime(graphdata['xaxis.range[0]'])
    if graphdata and 'xaxis.range[1]' in graphdata:
        todate = pd.to_datetime(graphdata['xaxis.range[1]'])
    return fromdate, todate


def historic(app, engine,
             tshclass=tsio.timeseries,
             serie_names=serie_names,
             routes_pathname_prefix='/tshistory/',
             request_pathname_prefix='/',
             cachedir=None):

    request_pathname_prefix_adv = request_pathname_prefix + routes_pathname_prefix

    external_stylesheets = [
        'https://codepen.io/chriddyp/pen/bWLwgP.css',
        'https://cdn.rawgit.com/plotly/dash-app-stylesheets/2d266c578d2a6e8850ebce48fdb52759b2aef506/stylesheet-oil-and-gas.css'
    ]

    if request_pathname_prefix != '/':
        dashboard = dash.Dash(
            'tsview',
            server=app,
            routes_pathname_prefix=routes_pathname_prefix,
            requests_pathname_prefix=request_pathname_prefix_adv,
            external_stylesheets=external_stylesheets
        )
    else:
        dashboard = dash.Dash(
            'tsview',
            server=app,
            url_base_pathname=routes_pathname_prefix,
            external_stylesheets=external_stylesheets
        )

    dashboard.config['suppress_callback_exceptions'] = True
    if request_pathname_prefix != '/':
        dashboard.config.requests_pathname_prefix = request_pathname_prefix_adv
    dashboard.css.append_css(
        {'external_url': 'https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css'}
    )

    if cachedir:
        cacheconfig = {
            'CACHE_TYPE': 'filesystem',
            'CACHE_DIR': cachedir
        }
    else:
        cacheconfig = {'CACHE_TYPE': 'simple'}
    cache = Cache(dashboard.server, config=cacheconfig)
    cache.init_app(dashboard.server)

    dashboard.layout = Div([
        Location(id='url', refresh=False),

        #Placeholders
        Div(Dropdown(id='ts_selector', value=None),
            style={'display': 'none'}),
        Div(Dropdown(id='idate_slider', value=None),
            style={'display': 'none'}),
        Div(Button(id='submit-button',
                   n_clicks=0,
                   children='Submit'),
            style={'display': 'none'}),

        Div(id='dropdown-container',
            style={'margin-top': '10'}),

        Graph(id='ts_snapshot'),

        Div(id='button-container'),
        Graph(id='ts_by_appdate',
              hoverData={'points':[{'text' : None}]}
        ),
        html.P('Select an insertion date:',
               style={'font-family': 'Helvetica',
                      "font-size": "100%",
                      'text-align': 'center',
                      }
               ),
        Div(id='slider-container'),
        Div([Br()]),
        Div([Br()]),
        html.P('Place the mouse on the graph above to see the republications for one application date:',
               style={'font-family': 'Helvetica',
                      "font-size": "100%",
                      'text-align': 'center',
                      }
               ),
        Graph(id='ts_by_insertdate'),
    ])

    @cache.memoize(timeout=300)
    def _get_diffs(id_serie, fromdate, todate):
        tsh = tshclass()
        with engine.begin() as cn:
            return {
                # canonicalize the keys immediately
                dt.strftime('%Y-%m-%d %H:%M:%S'): serie
                for dt, serie in tsh.history(
                        cn, id_serie,
                        from_value_date=fromdate,
                        to_value_date=todate
                ).items()
                if len(serie)
            }

    def get_diffs(id_serie, fromdate=None, todate=None):
        diffs = _get_diffs(id_serie, fromdate, todate)
        assert diffs is not None, (id_serie, fromdate, todate)
        return diffs

    def insertion_dates(id_serie, fromdate=None, todate=None):
        return list(
            get_diffs(id_serie, fromdate, todate).keys()
        )

    @dashboard.callback(dash.dependencies.Output('dropdown-container', 'children'),
                        [dash.dependencies.Input('url', 'pathname')])
    def adaptable_dropdown(url_string):
        all_names = serie_names(engine)
        formated_names = [
            {'label': name, 'value': name}
            for name in all_names
        ]

        if (url_string in (routes_pathname_prefix, request_pathname_prefix_adv) or
            url_string is None or
            len(url_string.strip('/')) == 0):
            initial_value = ''
        else:
            initial_value = url_string.split('/')[-1]
        dropdown = Dropdown(
            id='ts_selector',
            options=formated_names,
            value=initial_value
        )
        return dropdown

    @dashboard.callback(dash.dependencies.Output('slider-container', 'children'),
                        [dash.dependencies.Input('ts_selector', 'value'),
                         dash.dependencies.Input('submit-button', 'n_clicks')],
                        [dash.dependencies.State('ts_snapshot', 'relayoutData')])
    def adaptable_slider(id_serie, n_clicks, graphdata):
        if n_clicks==0:
            return Slider(id='idate_slider', value=None)

        fromdate, todate = unpack_dates(graphdata)
        idates = insertion_dates(id_serie, fromdate, todate)
        showlabel = len(idates) < 25
        slider = Slider(
            id='idate_slider',
            min=0,
            max=len(idates) - 1,
            value=len(idates) - 1,
            step=None,
            marks={
                str(idx): elt if showlabel else ''
                for idx, elt in enumerate(idates)
            }
        )
        return slider

    @dashboard.callback(dash.dependencies.Output('button-container', 'children'),
                        [dash.dependencies.Input('ts_selector', 'value')])
    def dynamic_button(_):
        return Button(
            id='submit-button',
            n_clicks=0,
            children='Show Republications',
            style = {'width': '100%',
                     'background-color': '#F7F7F7',
                     }),


    @dashboard.callback(dash.dependencies.Output('ts_snapshot', 'figure'),
                        [dash.dependencies.Input('ts_selector', 'value')])
    def ts_snapshot(id_serie):
        tsh = tshclass()
        ts = tsh.get(engine, id_serie)

        if id_serie is None or ts is None:
            return {'data': [], 'layout': {}}

        trace = [
            go.Scatter(
                x=ts.index,
                y=ts.values,
                name= id_serie,
                mode='lines',
                line={'color': ('rgb(255, 127, 80)')}
            )
        ]
        layout = go.Layout({
            'yaxis': {
                'fixedrange': True
            },
            'showlegend': True,
            'title' : 'Zoom to select the date interval:'

        })

        return {
            'data': trace,
            'layout': layout
        }


    @dashboard.callback(dash.dependencies.Output('ts_by_appdate', 'figure'),
                        [dash.dependencies.Input('idate_slider', 'value'),
                         dash.dependencies.Input('submit-button', 'n_clicks')],
                        [dash.dependencies.State('ts_selector', 'value'),
                         dash.dependencies.State('ts_snapshot', 'relayoutData')
                        ])
    def ts_by_appdate(idx, n_clicks, id_serie, graphdata):
        if n_clicks == 0:
            return {
                'data': [],
                'layout': {}
            }

        fromdate, todate = unpack_dates(graphdata)
        idx = idx if idx else 0

        tsh = tshclass()
        ts_final = tsh.get(engine, id_serie,
                           from_value_date=fromdate,
                           to_value_date=todate)
        ts_diff = get_diffs(id_serie, fromdate, todate)
        idates = insertion_dates(id_serie, fromdate, todate)

        if idx > len(idates):
            # may happen when the input div are not refreshed at the same time
            idx = len(idates) - 1

        insert_date = idates[idx]
        ts_unti_now = ts_diff[insert_date]
        # ts_until_now is the ts asof if diff='all'
        # else it represents the actall diff

        traces = []
        # all diffs
        for idate in idates:
            diff = ts_diff[idate]
            # plolty does not plot a line with only one point
            mode = 'lines' if len(diff) > 1 else 'markers'
            color = COLOR_BEFORE if idate < insert_date else COLOR_AFTER
            traces.append(
                go.Scatter(
                    x=diff.index,
                    y=diff.values,
                    text=[str(elt) for elt in diff.index],
                    name=idate,
                    showlegend=False,
                    mode=mode,
                    line={'color':color},
                    opacity=0.2
                )
            )

        # final snapshot
        traces.append(go.Scatter(
            x=ts_final.index,
            y=ts_final.values,
            name='last',
            text=[str(elt) for elt in ts_final.index],
            mode='lines',
            line={'color': COLOR_LAST},
            opacity=1
        ))

        # ts as of
        mode = 'lines' if len(ts_unti_now) > 1 else 'markers'
        traces.append(go.Scatter(
            x=ts_unti_now.index,
            y=ts_unti_now.values,
            text=[str(elt) for elt in ts_unti_now.index],
            name=str(pd.to_datetime(insert_date)),
            mode=mode,
            line={'color': COLOR_CURRENT},
        ))

        diffminvalue = min(
            diff.values.min()
            for diff in ts_diff.values()
            if len(diff)
        )
        diffmaxvalue = max(
            diff.values.max()
            for diff in ts_diff.values()
            if len(diff)
        )

        return {
            'data': traces,
            'layout': go.Layout(
                hovermode='closest',
                xaxis={'range': [ts_final.index.min(), ts_final.index.max()]},
                yaxis={'range': [diffminvalue, diffmaxvalue]},
                title='{} <br>Insertion date : {}'.format(id_serie, insert_date),
                shapes=[{
                    'type': 'line',
                    'x0': insert_date,
                    'y0': diffminvalue,
                    'x1': insert_date,
                    'y1': diffmaxvalue,
                    'line': {
                        'dash': 'dot',
                        'color': 'rgb(0, 0, 0)',
                        'width': 1
                    }
                }]
            )
        }

    @dashboard.callback(dash.dependencies.Output('ts_by_insertdate', 'figure'),
                        [dash.dependencies.Input('ts_by_appdate', 'hoverData')],
                        [dash.dependencies.State('ts_selector', 'value'),
                         dash.dependencies.State('ts_snapshot', 'relayoutData')
                         ])
    def ts_by_insertdate(hoverdata, id_serie, graphdata):
        if id_serie is None:
            return {
                'data': [],
                'layout': {}
            }

        date_str = hoverdata['points'][0]['text']
        if date_str is None:
            return {
                'data': [go.Scatter()],
                'layout': go.Layout()
            }

        fromdate, todate = unpack_dates(graphdata)

        ts_diff = get_diffs(id_serie, fromdate, todate)
        index = []
        values = []

        lastvalue = None
        dt = pd.to_datetime(date_str)
        for idate, diff in sorted(ts_diff.items()):
            if dt in diff.index:
                value = diff[dt]
                if value == lastvalue:
                    continue
                index.append(idate)
                values.append(value)
                lastvalue = value

        traces = [
            go.Scatter(
                x=index,
                y=values,
                name=date_str,
                mode='lines',
                line={'color': ('rgb(20, 180, 40)')},
            )
        ]

        return {
            'data': traces,
            'layout': go.Layout(
                hovermode='closest',
                title='Application date : {}'.format(date_str),
            )
        }
