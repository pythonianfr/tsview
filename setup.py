from setuptools import setup


setup(name='tsview',
      version='0.1',
      author='Pythonian',
      author_email='aurelien.campeas@pythonian.fr',
      description='Visualize time series from a `tshistory` repository',

      packages=['tsview'],
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
      ]}
)
