#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
发电数据预测

"""

import pandas as pd
import numpy as np
import datetime as dt

from trial1_fun import * # 调用写好的function
# run trial1_fun.py

# read data ---------------
dff = pd.read_csv("dff.csv")
dff.time = pd.to_datetime(dff.time)

# check the balance
dff.iny.sum()
dff.outy.sum()

# add columns
MCdf = dff.copy()

MCdf.ix[35135,0] = MCdf.iloc[35134,0] + dt.timedelta(minutes=15)
MCdf.ix[35135,1:3] = MCdf.iloc[35129:35135,1:3].apply(np.mean, axis=0)
MCdf =  addtime(MCdf)
# check if every day is completed
(MCdf.groupby("date").count().time != 96).sum()
# check if it is sorted
(MCdf.time.diff()<dt.timedelta(0)).sum()

# train and test --------------
# impute 0 values
MCdf.describe()
np.where(MCdf.outy==0)
for i in np.where(MCdf.outy==0)[0]:
    MCdf.outy.iloc[i] = MCdf.outy.iloc[(i-5):(i)].mean()
    
# add lag data
x,y,xtest,ytest = addlag_split(MCdf, range(96*1, 96*22))

# x = x[(-15000):(-1)]
# y = y[(-15000):(-1)]

## xgboost -----------
import xgboost as xgb
# read in data
dtrain = xgb.DMatrix(x, label=y)
dtest = xgb.DMatrix(xtest, label=ytest)
param = {'max_depth':6, 'eta':0.3}
num_round = 14
bst = xgb.train(param, dtrain, num_round)
# %timeit xgb.train(param, dtrain, num_round) 11.2 s per loop
Errmodel(bst,dtrain,dtrain.get_label(),dtest,dtest.get_label())
#           test      train
#RMSE  42.614662  34.221901
#MAPE   0.117633   0.105898
sorted(bst.get_score().items(), key=lambda x: x[1])
# ('hour', 7),
# ('lag97', 9),
# ('lag98', 9),
# ('daypart', 12),
# ('lag576', 14), 6
# ('lag1344', 16), 14
# ('lag672', 17), 7
# ('weekdays', 18), 
# ('lag2016', 21), 21
# ('lag96', 44)] 1

## LM ---------------
from sklearn.linear_model import LinearRegression
lr = LinearRegression(fit_intercept=True, normalize=False, copy_X=True, n_jobs=-1)
lr.fit(x, y)
# %timeit lr.fit(x, y) 13.8 s
Errmodel(lr,x,y,xtest,ytest)
#           test      train
#RMSE  42.859574  39.508639
#MAPE   0.130772   0.126182

## Random Forest --------------

from sklearn.ensemble import RandomForestRegressor
rf = RandomForestRegressor(n_estimators=10, criterion='mse', 
                           max_depth=6, min_samples_split=2, 
                           min_samples_leaf=1, min_weight_fraction_leaf=0.0, 
                           max_features='auto', max_leaf_nodes=None, 
                           min_impurity_split=1e-07, bootstrap=True, 
                           oob_score=False, n_jobs=-1, random_state=None, 
                           verbose=2, warm_start=False)
rf.fit(x, y)
# %timeit rf.fit(x, y) 1min 4s
Errmodel(rf,x,y,xtest,ytest)
#           test      train
#RMSE  43.223528  42.013826
#MAPE   0.120486   0.127029

