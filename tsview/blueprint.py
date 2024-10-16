import io
import csv
import json
import traceback
from collections import defaultdict
from contextlib import redirect_stdout

from flask import (
    Blueprint,
    jsonify,
    make_response,
    request,
    render_template,
    url_for,
    Response,
)
import numpy as np
import pandas as pd
from pml import HTML

from psyl.lisp import (
    parse as fparse,
    serialize,
)

from rework_ui.helper import argsdict as _args

from sqlhelp import select

from tshistory import (
    search,
    util
)
from tshistory.util import find_first_uriname

from tshistory_formula.helper import (
    BadKeyword,
    validate
)
from tshistory_formula.registry import (
    AUTO,
    FUNCS
)
from tshistory_formula.interpreter import jsontypes

from tsview.util import (
    argsdict as _argsdict,
    format_formula
)
from tsview.menu import definition as menu_spec
from tsview.moment import ConfigurationError
from tsview.icons import definition as icons_definition
from tsview.horizon import Horizon


def primary_names(tsa):
    cat = list(tsa.catalog(
        allsources=False
    ).values())[0]
    return sorted(
        name
        for name, kind in cat
        if kind == 'primary'
    )


def homeurl():
    homeurl = url_for('tsview.home')
    baseurl = homeurl[:homeurl.rindex('/')]
    if len(baseurl):
        return baseurl
    return baseurl


def has_roles(*required_roles):
    role = request.environ.get('ROLE') or 'guest'
    return role in required_roles


def validate_formula(tsa, df_formula):
    errors = defaultdict(list)
    warnings = defaultdict(list)

    ok = set()
    syntax_error = set()
    missing = set()

    # conflicts with primary series are an error
    primaries = {
        name for name in np.unique(df_formula['name'])
        if tsa.type(name) == 'primary'
            and tsa.exists(name)
    }

    # overriding an existing formula yields a warning
    formulas = {
        name for name in np.unique(df_formula['name'])
        if tsa.type(name) == 'formula'
            and tsa.exists(name)
    }

    uploadset = {
        row.name
        for row in df_formula.itertuples()
    }

    def exists(sname):
        if not tsa.exists(sname):
            if sname in AUTO:
                return True
            return False
        return True

    for row in df_formula.itertuples():
        # formula syntax error detection
        try:
            parsed = fparse(row.text)
            try:
                validate(parsed)
            except BadKeyword as error:
                syntax_error.add(row.name + ' : ' + repr(error))
                continue

        except SyntaxError:
            syntax_error.add(row.name)
            continue

        # and needed series
        needset = set(
            tsa.tsh.find_metas(tsa.engine, parsed)
        )
        # even if ok, the def might refer to the current
        # uploaded set, or worse ...
        newmissing = {
            needname
            for needname in needset
            if needname not in uploadset and not exists(needname)
        }
        missing |= newmissing
        if not newmissing:
            ok.add(row.name)

            # and last but not least.. tz compatibility
            # we need to check if the needed series exist otherwise it will raise a tz error
            need_uploadset = {
                needname
                for needname in needset
                if needname in uploadset and not exists(needname)
            }
            if not need_uploadset:
                try:
                    tsa.tsh.check_tz_compatibility(tsa.engine, parsed)
                except Exception as error:
                    syntax_error.add(row.name + ' : ' + repr(error))

    if primaries:
        errors['primary'] = sorted(primaries)

    if formulas:
        warnings['existing'] = sorted(formulas)

    if syntax_error:
        errors['syntax'] = sorted(syntax_error)

    if missing:
        errors['missing'] = sorted(missing)

    return errors, warnings


def tsview(tsa):
    bp = Blueprint(
        'tsview',
        __name__,
        template_folder='tsview_templates',
        static_folder='tsview_static',
    )

    @bp.route('/')
    def homepage():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        baseurl = homeurl()
        appname = find_first_uriname()
        instance = "Local" if len(appname)==0 else appname.capitalize()
        flags_menu = json.dumps([homeurl(), 'navigation-home'])
        import tshistory_refinery
        version = tshistory_refinery.__version__
        return render_template(
            'homepage.html',
            flags=json.dumps([baseurl, instance, version]),
            flags_menu=flags_menu,
            title = appname.title()
        )

    @bp.route('/tsview')
    def home():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        flags_menu = json.dumps([homeurl(), 'timeseries-quickview'])
        min = request.args.get('startdate')
        max = request.args.get('enddate')
        debug = request.args.get('debug')
        title = 'Quick view'
        return render_template(
            'tsview.html',
            homeurl=homeurl(),
            haseditor=json.dumps(False),  # NOTE: really fix me
            series=request.args.getlist("series"),
            min=min,
            max=max,
            debug=debug,
            flags_menu=flags_menu,
            title=title,
        )

    class logargs(_argsdict):
        defaults = {
            'series': None,
            'seriesvocab': lambda: primary_names(tsa)
        }
        types = {
            'series': str,
        }

    @bp.route('/tsviewlog')
    def tsviewlog():
        args = logargs(request.args)
        return render_template('tslog.html', **args)

    @bp.route('/tslog')
    def tslog():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        args = logargs(request.args)
        log = tsa.log(
            name=args.series
        )

        if not log:
            return 'No result.'

        return pd.DataFrame(reversed(log)).to_html(index=False)

    @bp.route('/tsdelete')
    def tsdelete():
        if has_roles('admin', 'rw'):
            flags_menu = json.dumps([homeurl(), 'timeseries-delete'])
            return render_template(
                'tsedit.html',
                edit_kind="Delete",
                homeurl=homeurl(),
                flags_menu=flags_menu,
                title='Delete'
            )

        return 'You do not have the delete capability.'

    @bp.route('/formulacache')
    def formulacache():
        if has_roles('admin', 'rw', 'ro'):
            flags_menu = json.dumps([homeurl(), 'formula-cache'])
            return render_template(
                'cache.html',
                homeurl=homeurl(),
                flags_menu=flags_menu,
                title='Cache'
            )

    # formula editor

    @bp.route('/spec')
    def spec():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        return json.dumps(
            sorted(
                [
                    (op_name, list(op_spec.items()))
                    for op_name, op_spec in json.loads(jsontypes()).items()
                ],
                key=lambda x: ("\x00",) if x[0] == 'series' else x
            )
        )

    @bp.route('/tsformula')
    def tsformula():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        name = request.args.get('name')
        formula = None
        if name:
            formula = dict(
                name=name,
                code=tsa.formula(name) or ''
            )
        flags_menu = json.dumps([homeurl(), 'formula-create'])
        title = 'Form: edit'
        return render_template(
            'tsformula.html',
            homeurl=homeurl(),
            spec=spec(),
            formula=json.dumps(formula),
            flags_menu=flags_menu,
            title=title
        )

    @bp.route('/tsformula/pygmentize', methods=['POST'])
    def tsformula_pygmentize():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        return json.dumps(
            format_formula(
                request.data.decode('utf-8'),
                baseurl=homeurl()
            )
        )

    @bp.route('/tsformula/spec-operators')
    def serve_operators():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        funcs = {
            name: (FUNCS[name].__doc__,
                   FUNCS[name].__module__)
            for name in sorted(FUNCS)
            if FUNCS[name].__doc__ is not None
        }
        to_send = []
        for (name, (doc, source)) in funcs.items():
            to_send.append(
                {
                    'name': name,
                    'doc': doc,
                    'source': source
                }
            )
        return json.dumps(to_send)

    @bp.route('/tsformula/operators')
    def formula_operators():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        home_url = homeurl() or "/"
        flags_menu = json.dumps([home_url, 'formula-documentation'])
        title = 'Form: doc'
        return render_template(
            'operators.html',
            baseurl=home_url,
            flags_menu=flags_menu,
            title=title,
        )

    @bp.route('/tsformula/try')
    def tryformula():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        # here we take a big bad shortcut ...
        # we want to think on how to expose that
        # in tshistory rest api
        # and for all sources ...
        # for now we are single source only

        return tsa.eval_formula(
            request.args['formula']
        ).to_json(
            orient='index',
            date_format='iso'
        )

    @bp.route('/addformulas')
    def addformulas():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        baseurl = homeurl()
        flags_menu = json.dumps([homeurl(), 'formula-batch'])
        title = 'Form: batch'
        return render_template(
            'addformulas.html',
            baseurl=baseurl,
            flags_menu=flags_menu,
            title=title,
        )

    @bp.route('/updateformulas', methods=['PUT'])
    def updateformulas():
        if not request.files:
            return jsonify({'errors': ['Missing CSV file']})

        args = _args(request.form)
        stdout = io.StringIO()
        output = []
        try:
            content = request.files.to_dict()['new_formula.csv'].stream.read().decode("utf-8")
            stdout.write(content)
            stdout.seek(0)
            df_formula = pd.read_csv(
                stdout,
                dtype={'name': str, 'text': str},
                sep=',',
                escapechar="/"
            )
            if ('name' not in df_formula.columns) or ('text' not in df_formula.columns):
                return jsonify({
                    'status' : 'invalid',
                    'errors': {'syntax' : ["Invalid file. Requested columns are ['name', 'text']"]},
                    'warnings': {},
                    'crash' : '',
                    'output' : {}
                })
            errors, warnings = validate_formula(tsa, df_formula)
            if errors or not args.reallydoit:
                status = 'invalid' if errors else 'valid'
                print(jsonify({
                    'status': status,
                    'errors': errors,
                    'warnings': warnings,
                    'crash' : '',
                    'output' : {}
                }))
                return jsonify({
                    'status': status,
                    'errors': errors,
                    'warnings': warnings,
                    'crash' : '',
                    'output' : {}
                })

            with redirect_stdout(stdout):
                for row in df_formula.itertuples():
                    tsa.register_formula(
                        row.name,
                        row.text,
                        reject_unknown=False
                    )
                    output.append(row.name + ' : ' + row.text)

        except Exception:
            traceback.print_exc()
            h = HTML()
            return json.dumps({
                'status': 'fail',
                'crash': str(h(traceback.format_exc())),
                'output': {},
                'errors' : {},
                'warnings' : {}
            })
        return jsonify({
            'status': 'saved',
            'output': {'Registered' : output},
            'crash': '',
            'errors' : {},
            'warnings' : {}
        })

    @bp.route('/downloadformulas')
    def downloadformulas():
        sql = (
            'select name, internal_metadata->\'formula\' as text '
            'from tsh.registry '
            'where internal_metadata->\'formula\' is not null'
        )
        formulas = pd.DataFrame(
            tsa.engine.execute(sql).fetchall()
        )
        df = formulas.sort_values(
            by=['name', 'text'],
            kind='mergesort'
        )
        df['text'] = df['text'].apply(lambda x: serialize(fparse(x)))
        response = make_response(
            df.to_csv(
                index=False,
                quoting=csv.QUOTE_NONE,
                quotechar="'",
                escapechar="/"
            ), 200
        )
        response.headers['Content-Type'] = 'text/json'

        return response

    # query editor

    @bp.route('/queryspec')
    def queryspec():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        return jsonify(_queryspec())

    def _queryspec():
        util.ensure_plugin_registration()
        types = {}
        for lispname, kname in search._OPMAP.items():
            cls = search.query.klassbyname(kname)
            # type object 'hascachepolicy' has no attribute '__sig__'
            if hasattr(cls, '__sig__'):
                types[lispname] = cls.__sig__()

        return_first = lambda x: 0 if x[0] == 'return' else 1  # noqa
        return [
            (op_name, sorted(op_spec.items(), key=return_first))
            for op_name, op_spec in types.items()
        ]

    @bp.route('/queryeditor')
    def queryeditor():
        if not has_roles('admin', 'rw'):
            return 'Nothing to see there.'
        flags_menu = json.dumps([homeurl(), 'basket-edit'])
        title = 'Basket: editor'
        return render_template(
            'queryeditor.html',
            homeurl=homeurl(),
            spec=json.dumps(_queryspec()),
            flags_menu=flags_menu,
            title=title,
        )

    # info

    @bp.route('/tsinfo')
    def tsinfo():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        flags_menu = json.dumps([homeurl(), 'timeseries-catalog'])
        name = request.args.get('name')
        min = request.args.get('startdate')
        max = request.args.get('enddate')
        debug = request.args.get('debug')
        title = f'Info: {name}'
        return render_template(
            'tsinfo.html',
            homeurl=homeurl(),
            name=name,
            flags_menu=flags_menu,
            min=min,
            max=max,
            debug=debug,
            title=title,
        )

    @bp.route('/groupinfo')
    def groupinfo():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        flags_menu = json.dumps([homeurl(), 'timeseries-catalog'])
        name = request.args.get('name')
        title = f'Group: {name}'
        return render_template(
            'groupinfo.html',
            homeurl=homeurl(),
            name=name,
            flags_menu=flags_menu,
            title=title,
        )

    @bp.route('/tsinfo/canwrite')
    def canwrite():
        return json.dumps(
            has_roles('admin', 'rw')
        )

    # catalog

    @bp.route('/tssearch')
    def tssearch():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        base_url = homeurl()
        flags_menu = json.dumps([base_url, 'timeseries-catalog'])
        title = 'Catalog'
        return render_template(
            'tssearch.html',
            homeurl=homeurl(),
            flags_menu=flags_menu,
            title=title,
        )

    @bp.route('/tssearch/allmetadata')
    def all_series_metadata():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        # here we take a big bad shortcut ...
        # we want to think on how to expose that
        # in tshistory rest api
        # and for all sources ...
        # for now we are single source only
        engine = tsa.engine

        q = select(
            'name', 'internal_metadata', 'metadata'
        ).table(
            f'"{tsa.namespace}".registry'
        )

        m = {
            name: imeta | (meta or {})
            for name, imeta, meta in q.do(engine).fetchall()
        }

        return jsonify(m)

    @bp.route('/tssearch/allformula')
    def all_series_formulas():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        # here we take a big bad shortcut ...
        # we want to think on how to expose that
        # in tshistory rest api
        # and for all sources ...
        # for now we are single source only
        engine = tsa.engine

        q = select(
            'name', 'internal_metadata->\'formula\''
        ).table(
            f'"{tsa.namespace}".registry'
        ).where(
            'internal_metadata->\'formula\' is not null'
        )

        return jsonify(
            {
                name: formula
                for name, formula in q.do(engine).fetchall()
            }
        )

    @bp.route('/groupsearch/allformula')
    def all_group_formulas():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        engine = tsa.engine
        q = select(
            'name', 'internal_metadata->\'formula\''
        ).table(
            f'"{tsa.namespace}".group_registry'
        ).where(
            'internal_metadata->\'formula\' is not null'
        )

        return jsonify(
            {
                name: formula
                for name, formula in q.do(engine).fetchall()
            }
        )

    @bp.route('/groupsearch/allmetadata')
    def all_group_metadata():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        engine = tsa.engine

        q = select(
            'name', 'internal_metadata', 'metadata'
        ).table(
            f'"{tsa.namespace}".group_registry'
        )

        m = {
            name: imeta | (meta or {})
            for name, imeta, meta in q.do(engine).fetchall()
        }

        return jsonify(
            dict(m)
        )

    @bp.route('/tseditor')
    def tseditor():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'

        name = request.args.get('name')
        min = request.args.get('startdate')
        max = request.args.get('enddate')
        debug = request.args.get('debug')
        flags_menu = json.dumps([homeurl(), 'timeseries-catalog'])
        title = f'Edit: {name}'
        return render_template(
            'tseditor.html',
            homeurl=homeurl(),
            name=name,
            min=min,
            max=max,
            debug=debug,
            flags_menu=flags_menu,
            title=title,
        )

    @bp.route('/formula-components')
    def formula_components():
        from tshistory_formula.helper import (
            replace_findseries,
            find_autos,
        )
        seriesname = request.args.get('name')
        assert tsa.exists(seriesname)
        tree = replace_findseries(
            tsa.engine,
            tsa.tsh,
            fparse(tsa.formula(seriesname)),
        )
        autos = find_autos(tsa.engine, tsa.tsh, seriesname)
        infos = [
            {
                'name': name,
                'type': tsa.type(name)
            }
            for name, expr in tsa.tsh.find_series(tsa.engine, tree).items()
        ]
        for list_auto in autos.values():
            for auto in list_auto:
                infos.append(
                    {
                        'name': auto[0],
                        'type': 'auto'
                    }
                )
        return json.dumps(infos)

    @bp.route('/settings')
    def route_settings():
        flags_menu = json.dumps([homeurl(), 'monitor-settings'])
        return render_template(
            'settings.html',
            homeurl=homeurl(),
            flags_menu=flags_menu,
        )

    # menu

    @bp.route('/menu')
    def serve_menu():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        full_url = url_for('tsview.home', _external=True)
        full_url = full_url[:full_url.rindex('/')]
        dashboard_url = full_url.replace('refinery', 'dashboard')
        return json.dumps(
            menu_spec(
                dashboard_url=dashboard_url
            )
        )

    @bp.route('/icons')
    def serve_icons():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        return json.dumps(icons_definition)

    @bp.route('/formulas')
    def formulas():
        if not has_roles('admin', 'rw', 'ro'):
            return 'Nothing to see there.'
        flags_menu = json.dumps([homeurl(), 'formula-catalog'])
        title = 'Form: list'
        return render_template(
            'formulas.html',
            homeurl=homeurl(),
            name=request.args.get('name'),
            flags_menu=flags_menu,
            title=title,
        )

    # horizon

    @bp.route('/horizon-choices')
    def horizon_choices():
        engine = tsa.engine
        api = Horizon(engine)
        return json.dumps(api.get_choices())

    @bp.route('/new-dates/<label>/<date_ref>/<step>')
    def new_dates(label, date_ref, step):
        engine = tsa.engine
        api = Horizon(engine)
        result = api.eval_bounds(
            label,
            date_ref,
            int(step)
        )
        return json.dumps(result)

    @bp.route('/translate-dates/<from_value_date>/<to_value_date>/<ref_date>/<step>')
    def translate_dates(from_value_date, to_value_date, ref_date, step):
        engine = tsa.engine
        api = Horizon(engine)
        result = api.translate(
            {
                'fromdate': from_value_date,
                'todate': to_value_date,
                'ref-date': ref_date,
            },
            int(step)
        )
        return json.dumps(result)

    @bp.route('/list-horizons')
    def list_horizons():
        engine = tsa.engine
        api = Horizon(engine)
        catalog = api.get_all()
        return json.dumps(
            catalog
        )

    @bp.route('/replace-horizons', methods = ['POST'])
    def replace_horizons():
        results = json.loads(request.data.decode())
        engine = tsa.engine
        api = Horizon(engine)
        try:
            api.replace_all(results)
            return Response(status=201)
        except ConfigurationError as err:
            return make_response(jsonify(error=str(err)), 500)

    return bp
