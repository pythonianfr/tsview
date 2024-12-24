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
    'C',
    'W',
    'WOM',
    'LWOM',
    'ME',
    'MS',
    'BME',
    'BMS',
    'CBME',
    'CBMS',
    'SME',
    'SMS',
    'QE',
    'QS',
    'BQE',
    'BQS',
    'REQ',
    'YE',
    'YS',
    'BYE',
    'BYS',
    'RE',
    'bh',
    'cbh',
    'D',
    'h',
    'min',
    's',
    'ms',
    'us',
    'ns'
]
