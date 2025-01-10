# import pandas as pd
# import pprint as pp
# df = pd.read_html('https://pandas.pydata.org/pandas-docs/stable/user_guide/timeseries.html')[2]
# result = []
# for val in df['Frequency String'].to_list():
#     if pd.isnull(val):
#         continue
#     val = val.split(' ')[0]
#     result.append(val.replace("""'""", """"""))
# pp.pprint(result)

OFFSETS = [
    'B',
    # 'C',
    'W',
    #'WOM', -> "Prefix \'WOM\' requires a suffix."
    #'LWOM',
    'ME',
    'MS',
    'BME', # Business month end
    'BMS', # Business month start
    # 'CBME',
    # 'CBMS',
    'SME', # every 15th and last day of the month
    'SMS', # every first day and 15th of the month
    'QE',
    'QS',
    'BQE',
    'BQS',
    # 'REQ', REQ, failed to parse with error message: TypeError('_parse_suffix() takes exactly 3 positional arguments (0 given)')"
    'YE',
    'YS',
    'BYE',
    'BYS',
    #'RE', RE, failed to parse with error message: TypeError('_parse_suffix() takes exactly 3 positional arguments (0 given)')")
    'bh', # business hours (from 9 to 16 every business day)
    # 'cbh',
    'D',
    'h',
    'min',
    's',
    # 'ms',
    # 'us',
    # 'ns'
]
