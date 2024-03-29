import uuid
import json
from warnings import warn

from werkzeug.datastructures import ImmutableMultiDict
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter
from plotly import utils

from psyl.lisp import parse, pretty


# formula helper

class DecoratingHtmlFormatter(HtmlFormatter):
    _serieshtml = '<span class="nv">series</span><span class="w"> </span><span class="s">'
    _integrationhtml = '<span class="nv">integration</span><span class="w">'

    def __init__(self, *args, baseurl='', **options):
        super().__init__(*args, **options)
        self._baseurl = baseurl

    def wrap(self, source, *, include_div=True):
        return self.modify_str_series(source)

    def patch_series(self, fragment):
        name = fragment.split('&quot;')[1]
        fragment = fragment.replace(
            '<span class="nv">series</span><span class="w"> </span><span class="s">',
            '<span class="nv">series</span><span class="w"> </span>'
            f'<a class="s" href="{self._baseurl}/tsinfo?name={name}">'
        )
        return fragment.replace('&quot;</span>', '&quot;</a>')

    def patch_integration(self, fragment):
        sn_number = fragment.count('&quot;')
        for i in range(1, sn_number, 2):
            sn = fragment.split('&quot;')[i]
            fragment = fragment.replace(
                '<span class="w"> </span><span class="s">',
                f'<span class="w"> </span>'
                f'<a class="s" href="{self._baseurl}/tsinfo?name={sn}">',
                1
            )
            fragment = fragment.replace('&quot;</span>', '&quot;</a>', 1)
        return fragment

    def modify_str_series(self, source):
        yield 0, '<pre>'
        yield 0, '<span></span>'
        integration_case = 0
        for i, t in source:
            # general case
            if self._serieshtml in t:
                t = self.patch_series(t)
            # case of integration operator in an expand
            if self._integrationhtml in t:
                t = self.patch_integration(t)
            # case of integration operator "solo"
            if integration_case > 0:
                integration_case = integration_case - 1
                sn = t.split('&quot;')[1]
                t = t.replace(
                    '<span class="w">    </span><span class="s">',
                    f'<span class="w">    </span>'
                    f'<a class="s" href="{self._baseurl}/tsinfo?name={sn}">'
                )
                t = t.replace('&quot;</span>', '&quot;</a>')
            if t == '<span class="p">(</span><span class="nv">integration</span>\n':
                integration_case = 2 # need to modify the two next iterations
            yield i, t
        yield 0, '</pre>'


def format_formula(formula, baseurl='', softbreak=90):
    return highlight(
        pretty(
            parse(formula),
            softbreak=softbreak
        ),
        get_lexer_by_name("fennel"),
        DecoratingHtmlFormatter(baseurl=baseurl)
    )


# webapp helper

class dictobj(dict):
    """ a dict-like object:
    * whose values can also be get/set using the `obj.key` notation
    * object[key] returns None if the key is not known
    """

    def __getattr__(self, name):
        return self[name]

    def __setattr__(self, name, value):
        self[name] = value

    def __getitem__(self, name):
        if name in self:
            return super(dictobj, self).__getitem__(name)
        return None

    def copy(self):
        return self.__class__((k, self[k]) for k in self)


class argsdict(dictobj):
    types = {}
    defaults = {}

    def __init__(self, reqargs=None, defaults=None, types=None):
        """ transforms the request args (or any such dict) for convenience :
        * be a malleable dictobj (whose missing attrs/keys default to None)
        * set the default values (if any, defaults is a mapping from keys
          to a scalar or a collable)
        * coerce to the wanted types (if any, types is a mapping from keys
          to a type or factory function)
        """
        super(argsdict, self).__init__()
        if reqargs is None:  # copy constructor
            return

        if not isinstance(reqargs, ImmutableMultiDict):
            for k, v in reqargs.items():
                self[k] = v
            self._set_defaults(defaults)
            return

        defaults = defaults or self.defaults
        types = types or self.types
        for key, val in reqargs.to_dict(flat=False).items():
            # when sending json, attributes land as `<attribute>[]`
            islist = key.endswith('[]')
            key = key.rstrip('[]')
            targettype = types.get(key)
            # signal if there is any discrepancy and force to tuple
            if islist and targettype not in (list, tuple):
                warn('element %r is a sequence but its expected type is %r' %
                     (key, targettype))
                targettype = tuple
            # val can be an str or a sequence of strs
            # hence `not filter(None, val)` gets us all
            # the falsy values ('', [''])
            if not list(filter(None, val)):  # py3k: force to list
                # no value -> default value
                default = defaults.get(key)
                self[key] = default() if callable(default) else default
            else:
                self[key] = val if targettype in (list, tuple) else val[0]
            # type coercion
            if targettype:
                self[key] = targettype(self[key])
        self._set_defaults(defaults)

    def _set_defaults(self, defaults=None):
        defaults = defaults or self.defaults
        # complete entries with mandatory defaults
        for key, val in defaults.items():
            if key not in self:
                self[key] = val() if callable(val) else val

    def copy(self):
        new = self.__class__()
        for k in self:
            new[k] = self[k]
        return new


# plotly helper

def plot_to_htmldiv(data, layout=None, divid=None):
    assert isinstance(data, (list, tuple))
    layout = layout or {}
    divid = divid or uuid.uuid4()

    script = ('Plotly.newPlot("%(id)s", %(data)s, %(layout)s, '
              '{displaylogo: false, modeBarButtonsToRemove: ["sendDataToCloud"]})' %
              dict(id=divid,
                   data=json.dumps(data,
                                   cls=utils.PlotlyJSONEncoder,
                                   sort_keys=True),
                   layout=json.dumps(layout,
                                     cls=utils.PlotlyJSONEncoder,
                                     sort_keys=True)))

    return (
        '<div id="{id}" '
        '     class="plotly-graph-div">'
        '</div>'
        '<script type="text/javascript">'
        '    window.PLOTLYENV=window.PLOTLYENV || {{}};'
        '    {script}'
        '</script>').format(
            id=divid,
            script=script,
        )
