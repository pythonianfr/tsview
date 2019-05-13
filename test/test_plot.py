from pathlib import Path
import pandas as pd

from tshistory.tsio import timeseries

from tsview.plot import plot
from tsview.util import argsdict


DATADIR = Path(__file__).parent / 'data'

def test_plot(engine, refresh):
    serie = pd.Series([1, 2, 3, 4, 5],
                      index=pd.date_range(start='2017-1-1',
                                          freq='D',
                                          periods=5))

    tsh = timeseries()

    with engine.begin() as cn:
        tsh.insert(cn, serie, 'banana_volume_consumption', 'Babar')

    args = argsdict({'series': ['banana_volume_consumption']})
    plotted = plot(args, engine, timeseries, divid='mydiv').strip().encode('utf-8')

    refpath = DATADIR / 'plotted.html'
    if refresh:
        refpath.write_bytes(plotted)
    assert refpath.read_bytes() == plotted
