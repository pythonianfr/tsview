import plotly.graph_objs as go

from tsview.util import plot_to_htmldiv


def plot(args, engine, tshclass, divid=None):
    tsh = tshclass()
    series = []

    with engine.begin() as cn:
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
    )

    return plot_to_htmldiv(traces, layout, divid)
