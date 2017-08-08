#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
比较不同分辨率下的预测精度。分辨率代表多长时间有一个数据点，比如分辨率是1小时代表每1小时有一个数据点。

"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
# import datetime as dt

from trial2_2_fun import * # 调用写好的所有函数。凡是spyder自动打感叹号的函数，都是出自这里面
#run trial2_fun.py

#%% 数据预处理：这部分和trial2一样的 ------------------
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

#%% 分辨率是15min，直接用之前跑好的结果
# result_1 = All_result
table_1 = analyze_result_table(result_table(result_1),points=96)

#%% 分辨率是 1 hour
df1 = df.copy()
df1.index = [i - pd.Timedelta(minutes=i.minute) for i in df1.index]
df1 = df1.groupby(df1.index).mean()
# result_4 = {}
MCexperiment(df1, ['xgb','lr'], result_4, lag_range=range(24*1,24*22))
table_4 = analyze_result_table(result_table(result_4),points=24)

#%% 分辨率是 2 hour
df1 = df.copy()
df1.index = [i - pd.Timedelta(minutes=i.minute,hours=i.hour%2) for i in df1.index]
df1 = df1.groupby(df1.index).mean()
# result_8 = {}
MCexperiment(df1, ['xgb','lr'], result_8, lag_range=range(12*1,12*22))
table_8 = analyze_result_table(result_table(result_8),points=12)

#%% 分辨率是 4 hour
df1 = df.copy()
df1.index = [i - pd.Timedelta(minutes=i.minute,hours=i.hour%4) for i in df1.index]
df1 = df1.groupby(df1.index).mean()
# result_16 = {}
MCexperiment(df1, ['xgb','lr'], result_16, lag_range=range(6*1,6*22))
table_16 = analyze_result_table(result_table(result_16),points=6)

#%% 分辨率是 8 hour
df1 = df.copy()
df1.index = [i - pd.Timedelta(minutes=i.minute,hours=i.hour%8) for i in df1.index]
df1 = df1.groupby(df1.index).mean()
# result_32 = {}
MCexperiment(df1, ['xgb','lr'], result_32, lag_range=range(3*1,3*22))
table_32 = analyze_result_table(result_table(result_32),points=3)

#%% 分辨率是 12 hour
df1 = df.copy()
df1.index = [i - pd.Timedelta(minutes=i.minute,hours=i.hour%12) for i in df1.index]
df1 = df1.groupby(df1.index).mean()
# result_48 = {}
MCexperiment(df1, ['xgb','lr'], result_48, lag_range=range(2*1,2*22))
table_48 = analyze_result_table(result_table(result_48),points=2)

#%% 分辨率是 1 day
df1 = df.copy()
df1.index = [i - pd.Timedelta(minutes=i.minute,hours=i.hour) for i in df1.index]
df1 = df1.groupby(df1.index).mean()
# result_96 = {}
MCexperiment(df1, ['xgb','lr'], result_96, lag_range=range(1*1,1*22))
table_96 = analyze_result_table(result_table(result_96),points=1)

#%% 分辨率是 7 days
df1 = df.copy()
df1.index = [i - pd.Timedelta(minutes=i.minute,hours=i.hour,days=i.weekday()) for i in df1.index]
df1 = df1.groupby(df1.index).mean()
# result_672 = {}
MCexperiment(df1, ['xgb','lr'], result_672, lag_range=range(1*1,1*10))
table_672 = analyze_result_table(result_table(result_672),points=1/7)

#%% 分析和作图
# 获取 test_MAPE 
all_table = [table_1,table_4,table_8,table_16, table_32,table_48,table_96,table_672]
del table_1,table_4,table_8,table_16, table_32,table_48,table_96,table_672
all_table_ana = pd.DataFrame({
        'mean_lr':[round(i['lr']['test_MAPE']['mean'],5) for i in all_table],
        'mean_xgb':[round(i['xgb']['test_MAPE']['mean'],5) for i in all_table],
        'median_lr':[round(i['lr']['test_MAPE']['50%'],5) for i in all_table],
        'median_xgb':[round(i['xgb']['test_MAPE']['50%'],5) for i in all_table],
        'std_lr':[round(i['lr']['test_MAPE']['std'],4) for i in all_table],
        'std_xgb':[round(i['xgb']['test_MAPE']['std'],4) for i in all_table]
        },index = ['15m','1h','2h','4h','8h','12h','1d','7d'])
#作图Mean and Median of Errors
all_table_ana.plot(y=['mean_lr','mean_xgb','median_lr','median_xgb'],alpha=0.5)
plt.xlabel('Time')
plt.ylabel('MAPE Error')
plt.title('Mean and Median of Errors')
#作图Standard Deviation of Errors
all_table_ana.plot(y=['std_lr','std_xgb'],alpha=0.5)
plt.xlabel('Time')
plt.ylabel('Standard Deviation')
plt.title('Standard Deviation of Errors')
#作图Errors of model lr(Linear Regression)
all_result = [result_1,result_4,result_8,result_16, result_32,result_48,result_96,result_672]
pd.DataFrame(np.array([[rst[i]['lr']['confusion']['test']['MAPE'] for i in sorted(rst)] for rst in all_result]),
             index = ['15m','1h','2h','4h','8h','12h','1d','7d']).plot()
plt.xlabel('Time')
plt.ylabel('MAPE Error')
plt.title('Errors of model lr(Linear Regression)')
#作图Errors of model xgboost
pd.DataFrame(np.array([[rst[i]['xgb']['confusion']['test']['MAPE'] for i in sorted(rst)] for rst in all_result]),
             index = ['15m','1h','2h','4h','8h','12h','1d','7d']).plot()
plt.xlabel('Time')
plt.ylabel('MAPE Error')
plt.title('Errors of model xgboost')
# 分析哪些用电单位的MAPE会比较大
_data = pd.DataFrame(np.array([[rst[i]['xgb']['confusion']['test']['MAPE'] for i in sorted(rst)] for rst in all_result]),
             index = ['15m','1h','2h','4h','8h','12h','1d','7d']).T
_data.index = sorted(all_result[0])
_data['1d'].sort_values()
# 这个结果是找出MAPE比较大的用电单位。可以在plot文件夹中看他们的图像。

