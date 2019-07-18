import subprocess
from pathlib import Path
from setuptools import setup
from setuptools.command.build_ext import build_ext


def compile_elm(wdir, edit_kind):
    """Compile elm component to JS"""
    src = wdir / "elm" / f"{edit_kind}.elm"
    out = wdir / "tsview" / "tsview_static" / f"{edit_kind.lower()}_elm.js"
    cmd = f"elm make --output {out} {src}"
    print(cmd, subprocess.call(cmd, shell=True))


class ElmBuild(build_ext):
    """Build Elm components
    """

    def run(self):
        wdir = Path(__file__).resolve().parent
        for edit_kind in ["Delete", "Rename", "Plot"]:
            compile_elm(wdir, edit_kind)
        super().run()


setup(name='tsview',
      version='0.6.0',
      author='Pythonian',
      author_email='aurelien.campeas@pythonian.fr',
      description=('Plugin to `tshistory` which provides a `view` subcommand '
                   'to visualize time series from a repository and '
                   'a flask blueprint'),
      url='https://bitbucket.org/pythonian/tsview',
      packages=['tsview'],
      zip_safe=False,
      install_requires=[
          'flask',
          'flask-caching',
          'pytest_sa_pg',
          'tshistory',
          'plotly==3.1.1',
          'dash==1.0.2',
          'dash-core-components==1.0.0',
          'dash-renderer==1.0.0',
          'dash-html-components==1.0.0',
          'tshistory_rest'
      ],
      package_data={'tsview': [
          'tsview_static/*',
          'tsview_templates/*'
      ]},
      entry_points={'tshistory.subcommands': [
          'view=tsview.command:view'
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
