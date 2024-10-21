from tshistory.migrate import (
    Migrator as _Migrator,
    version
)

from tsview import __version__
from tsview.util import initialize_horizon

class Migrator(_Migrator):
    _order = 4
    _package_version = __version__
    _package = 'tsview'


@version('tsview', '0.20.0')
def create_schema(engine, namespace, interactive):
    initialize_horizon(engine.url, force=False)
