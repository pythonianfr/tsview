from setuptools import setup


setup(name='tsview',
      version='0.1',
      author='Pythonian',
      author_email='aurelien.campeas@pythonian.fr',
      description=('Plugin to `tshistory` which provides a `view` subcommand '
                   'to visualize time series from a repository'),
      packages=['tsview', 'tsview.app'],
      package_dir={'tsview': '.'},
      install_requires=[
          'flask',
          'plotly',
          'pytest_sa_pg',
          'tshistory'
      ],
      dependency_links=[
          'hg+https://bitbucket.org/pythonian/tshistory#egg=tshistory-0'
      ],
      package_data={'tsview': [
          'app/static/*',
          'app/templates/*'
      ]},
      entry_points={'tshistory.subcommands': [
          'view=tsview.command:view'
      ]}
)
