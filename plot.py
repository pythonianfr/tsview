import plotly.graph_objs as go

from tshistory.tsio import TimeSerie

from tsview.util import plot_to_htmldiv


def plot(args, engine, divid=None):
    tsh = TimeSerie()
    series = []

    with engine.connect() as cn:
        for name in args.series:
            series.append((name, tsh.get(cn, name)))

    traces = []
    for name, ts in series:
        traces.append(
            go.Scatter(
                name=name,
                x=ts.index,
                y=ts.values)
        )

    layout = go.Layout(
        showlegend=True,
        width=1400,
        height=900
    )

    return plot_to_htmldiv(traces, layout, divid)
