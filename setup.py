from setuptools import setup


setup(name='tsview',
      version='0.1.0',
      author='Pythonian',
      author_email='aurelien.campeas@pythonian.fr',
      description=('Plugin to `tshistory` which provides a `view` subcommand '
                   'to visualize time series from a repository'),
      url='https://bitbucket.org/pythonian/tsview',
      packages=['tsview'],
      zip_safe=False,
      install_requires=[
          'flask',
          'plotly',
          'pytest_sa_pg',
          'tshistory'
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
