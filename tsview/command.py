from threading import Thread
import socket
import webbrowser

import click

from tsview.app import kickoff


@click.command()
@click.argument('db-uri')
@click.option('--debug', is_flag=True, default=False)
def view(db_uri, debug=False):
    """visualize time series through the web"""
    ipaddr = socket.gethostbyname(socket.gethostname())
    port = 5678

    if debug:
        kickoff(ipaddr, port, db_uri)
        return

    server = Thread(name='tsview.webapp', target=kickoff,
                    kwargs={'host': ipaddr, 'port': port, 'dburi': db_uri})
    server.daemon = True
    server.start()

    webbrowser.open('http://{ipaddr}:{port}/tsview'.format(ipaddr=ipaddr, port=port))
    input()
