import subprocess
from pathlib import Path
from setuptools import setup
from setuptools.command.build_ext import build_ext

from tsview import __version__


WORKING_DIR = Path(__file__).resolve().parent

STATIC_DIR = WORKING_DIR / "tsview" / "tsview_static"


def compile_elm(edit_kind, src):
    """Compile elm component to JS"""
    src = WORKING_DIR / 'elm' / src
    out = STATIC_DIR / f'{edit_kind}_elm.js'
    cmd = f'elm make --optimize --output {out} {src}'
    subprocess.call(cmd, shell=True)


class ElmBuild(build_ext):
    """Build Elm components"""

    def run(self):
        for edit_kind, src in [
                ('homepage', 'Homepage.elm'),
                ('menu_stand_alone', 'StandAloneMenu.elm'),
                ('operators', 'Operators.elm'),
                ('qeditor', 'Queryeditor.elm'),
                ('delete', 'Delete.elm'),
                ('addformulas', 'Addformulas.elm'),
                ('formulas', 'Formulas.elm'),
                ('plot', 'Plot.elm'),
                ('editor_tree', Path('Editor/UI/Tree.elm')),
                ('tsinfo', 'Tsinfo.elm'),
                ('tseditor', 'Tseditor.elm'),
                ('groupinfo', 'Groupinfo.elm'),
                ('search', 'Search.elm'),
                ('cache', 'Cache.elm'),
        ]:
            compile_elm(edit_kind, src)
        css = STATIC_DIR / 'pygmentize.css'
        cmd = f'pygmentize -S default -f html -a .highlight > {css}'
        subprocess.call(cmd, shell=True)
        super().run()


doc = Path(__file__).parent / 'README.md'


setup(name='tsview',
      version=__version__,
      author='Pythonian',
      author_email=(
          'aurelien.campeas@pythonian.fr,'
          'andre.espaze@pythonian.fr,'
          'arnaud.campeas@pythonian.fr'
          'auguste.pezin@pythonian.fr'
      ),
      description=(
          'Plugin to `tshistory` which provides a `view` subcommand '
          'to visualize time series'
      ),
      long_description=doc.read_text(),
      long_description_content_type='text/markdown',
      url='https://hg.sr.ht/~pythonian/tsview',

      packages=['tsview'],
      zip_safe=False,
      install_requires=[
          'flask == 2.3.3',
          'werkzeug',
          'pytest_sa_pg',
          'tshistory >= 0.19.4',
          'plotly < 6.0',
          'tshistory_formula >= 0.15'
      ],
      package_data={'tsview': [
          'tsview_static/*',
          'tsview_templates/*'
      ]},
      entry_points={'tshistory.subcommands': [
          'view=tsview.cli:view'
      ]},
      classifiers=[
          'Development Status :: 4 - Beta',
          'Intended Audience :: Developers',
          'License :: OSI Approved :: GNU Lesser General Public License v3 (LGPLv3)',
          'Operating System :: OS Independent',
          'Programming Language :: Python :: 3',
          'Topic :: Database',
          'Topic :: Scientific/Engineering',
          'Topic :: Software Development :: Version Control',
          'Topic :: Scientific/Engineering :: Visualization'
      ],
      cmdclass={'build_ext': ElmBuild}
)
