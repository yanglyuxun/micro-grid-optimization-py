#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Use all data to test the effectiveness of the prediction

@author: Lyuxun Yang
"""

import pandas as pd
import numpy as np
# import datetime as dt

from trial2_fun import * # the functions file
#run trial2_fun.py

# data preparing ------------------
df = pd.read_csv("LD2011_2014.txt",sep=";",decimal=',',
                 header=0,engine='c', index_col = 0,
                 verbose=False)
df.index = pd.to_datetime(df.index)

zero_to_na(df) # replace NAN in place!!!

sorted(df.apply(lambda x: x.count()), reverse=1) # how many are not nan
#[16032,
# 28160,
# 28992,
# 30432,
# 30432,
# 31008,
# ...

# choose 28160 and drop 1 column
np.where(df.apply(lambda x: x.count()) <28160)
df = df.iloc[-28160:,]
df = df.dropna(axis=1)

# is there too many 0s?
sorted(df.apply(lambda x: (x==0).sum()))
# drop the ones with 0s more than 1000
df = df.drop(df.columns[df.apply(lambda x: (x==0).sum()>1000)], axis=1)

# MC experiment ---------------------
# method =  ['xgb','lr','rf']
# All_result = {}  # the global var to store results
MCexperiment(df, ['xgb','lr'], All_result, lag_range=range(96*1,96*22))
Table_results = result_table(All_result)
table_ana = analyze_result_table(Table_results)

# 15min
# Result_1 = {} 
MCexperiment(df, ['xgb','lr'], Result_1, lag_range=range(1,96*22))
table_1 = analyze_result_table(result_table(Result_1))

# 1h
# Result_4 = {}
MCexperiment(df, ['xgb','lr'], Result_4, lag_range=range(4,96*22))
table_4 = analyze_result_table(result_table(Result_4))

# 4h
# Result_16 = {}
MCexperiment(df, ['xgb','lr'], Result_16, lag_range=range(16,96*22))
table_16 = analyze_result_table(result_table(Result_16))

# 1d
# Result_961 = {} = All_result
#MCexperiment(df, ['xgb','lr'], Result_961, lag_range=range(96*1,96*22))
#table_961 = analyze_result_table(result_table(Result_961))

# 2d
# Result_962 = {}
MCexperiment(df, ['xgb','lr'], Result_962, lag_range=range(96*2,96*22))
table_962 = analyze_result_table(result_table(Result_962))

# 4d
# Result_964 = {}
MCexperiment(df, ['xgb','lr'], Result_964, lag_range=range(96*4,96*22))
table_964 = analyze_result_table(result_table(Result_964))

# get the test_MAPE 
[i['lr']['test_MAPE']['50%'] for i in [table_1,table_4,table_16,table_ana, table_962,table_964]]
[i['xgb']['test_MAPE']['50%'] for i in [table_1,table_4,table_16,table_ana, table_962,table_964]]


### analyze the importance
importance = table_1['importance']
len(importance.index[importance[0]>=20])
with open('vars.txt','w') as f:
    f.writelines([i+'\n' for i in importance.index[importance[0]>=20] if i.startswith('lag')])