from setuptools import setup


setup(name='tsview',
      version='0.2.1',
      author='Pythonian',
      author_email='aurelien.campeas@pythonian.fr',
      description=('Plugin to `tshistory` which provides a `view` subcommand '
                   'to visualize time series from a repository'),
      url='https://bitbucket.org/pythonian/tsview',
      packages=['tsview'],
      zip_safe=False,
      install_requires=[
          'flask',
          'flask-caching',
          'plotly',
          'pytest_sa_pg',
          'tshistory',
          'dash',
          'dash-core-components==0.22.1',
          'dash-renderer==0.12.1',
          'dash-html-components==0.10.0'
      ],
      package_data={'tsview': [
          'static/*',
          'templates/*'
      ]},
      entry_points={'tshistory.subcommands': [
          'view=tsview.command:view'
      ]},
      classifiers=[
          'Development Status :: 4 - Beta',
          'Intended Audience :: Developers',
          'License :: OSI Approved :: GNU Lesser General Public License v3 (LGPLv3)',
          'Operating System :: OS Independent',
          'Programming Language :: Python :: 2',
          'Programming Language :: Python :: 3',
          'Topic :: Database',
          'Topic :: Scientific/Engineering',
          'Topic :: Software Development :: Version Control',
          'Topic :: Scientific/Engineering :: Visualization'
      ]
)
