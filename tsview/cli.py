from os import getenv
from threading import Thread
import socket
import webbrowser

import click

from tshistory import api
from tshistory.util import find_dburi
from tshistory_formula.tsio import timeseries

from tsview.app import kickoff
from tsview.util import initialize_horizon


def host():
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.connect(('8.8.8.8', 1))
    return s.getsockname()[0]


@click.command()
@click.argument('db-uri')
@click.option('--debug', is_flag=True, default=False)
def view(db_uri, debug=False):
    """visualize time series through the web"""
    tsa = api.timeseries(
            find_dburi(db_uri),
            handler=timeseries
        )
    ipaddr = host()
    port = int(getenv('TSVIEW_PORT', 5678))

    if debug:
        kickoff(ipaddr, port, tsa)
        return

    server = Thread(
        name='tsview.webapp', target=kickoff,
        kwargs={
            'host': ipaddr,
            'port': port,
            'tsa': tsa,
        }
    )
    server.daemon = True
    server.start()

    webbrowser.open('http://{ipaddr}:{port}/tsview'.format(ipaddr=ipaddr, port=port))
    input()


@click.command('init-horizon')
@click.argument('db-uri')
@click.option('--force', is_flag=True, default=False)
def init_horizon(db_uri, force=False):
    initialize_horizon(db_uri, force)
