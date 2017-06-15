from threading import Thread
import socket
import webbrowser

import click

from tshistory import command

from tsview.app.webapp import kickoff

tsh = command.tsh


@tsh.command()
@click.argument('db-uri')
def view(db_uri):
    ipaddr = socket.gethostbyname(socket.gethostname())
    port = 5678
    server = Thread(name='tsview.webapp', target=kickoff,
                    kwargs={'host': ipaddr, 'port': port, 'dburi': db_uri})
    server.daemon = True
    server.start()

    webbrowser.open('http://{ipaddr}:{port}/tsview'.format(ipaddr=ipaddr, port=port))
    input()


if __name__ == '__main__':
    command.tsh()
