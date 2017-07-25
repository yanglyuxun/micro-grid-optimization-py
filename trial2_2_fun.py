#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
functions for "trial2_2.py"

@author: Lyuxun Yang
"""

import pandas as pd
import numpy as np
# import datetime as dt
import time
import random
import xgboost as xgb
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor

def zero_to_na(df):
    """
    The 0s from the begining are actually NAN
    """
    for j,col in enumerate(df):
        for i,it in enumerate(df[col]):
            if it!=0:
                break
        df.iloc[:i,j] = np.nan
        #print(j)

def addtime(df):
    '''
    add time vars to the dataFrame
    '''
    df = df.copy()
    # df.loc[:,"date"] = [x.date() for x in df.index]
    # df.loc[:,"month"] = [x.month for x in df.index]
    df.loc[:,"day"] = [x.day for x in df.index]
    # df.loc[:,"daypart"] = np.where(df.day<=10, 1, np.where(df.day<=20, 2, 3))
    df.loc[:,"weekdays"] = [x.weekday() for x in df.index]
    df.loc[:,"hour"] = [x.hour for x in df.index]
    df.loc[:,"minute"] = [x.minute for x in df.index]
    return(df)

def addlag_split(df, step_range, train_rate=0.7): # step_range = range(96,96*22)
    """
    Add lag vars in the data and split it into train set and test set
    """
    #n = step_range[-1]
    colnames = df.columns.to_native_types()
    colnames[0] = "outy"
    df.columns = colnames
    MCdfselect = pd.get_dummies(df, columns=["day","weekdays","hour","minute"])
    dflist = [MCdfselect]
    for i in step_range:
        dflist.append(MCdfselect.outy.shift(i).rename("lag" + str(i)))
    MCdfselect = pd.concat(dflist,axis=1)
    MCdfselect = MCdfselect.dropna()
    bound = int(train_rate * df.shape[0])
    traindf = MCdfselect.iloc[:bound,:]
    testdf = MCdfselect.iloc[bound:,:]
    x = traindf.drop(["outy"],axis=1)
    y = traindf.outy
    xtest = testdf.drop(["outy"],axis=1)
    ytest = testdf.outy
    return x,y,xtest,ytest

def Err(prediction, truevalue):
    '''
    calculate the RMSE, MAPE for some prediction
    '''
    RMSE = np.sqrt(((prediction - truevalue)**2).mean())
    n0s = sum(truevalue == 0)
    if n0s>0:
        print("There are some 0 values!")
        isnotzero = truevalue != 0
        prediction, truevalue = prediction[isnotzero], truevalue[isnotzero]
    MAPE = (np.abs(prediction - truevalue) / truevalue).mean()
    return [RMSE, MAPE, n0s]
def Errmodel(model,x,y,xtest,ytest, **other):
    '''
    calculate the error indexes for a model which has a .predict() method
    '''
    yp = model.predict(x, **other)
    time0 = time.time()
    ytestp = model.predict(xtest, **other)
    test_time = time.time()-time0
    return pd.DataFrame({"train":Err(yp,y),
                         "test":Err(ytestp,ytest)}, 
                        index=["RMSE","MAPE","n_0s"]), test_time

def MakePrediction(dff, method, lag_range = range(96*1,96*22)):
    '''
    make prediction for a DataFrame
    '''
    if type(method)==str:
        method = [method]
    result = {}
    x,y,xtest,ytest = addlag_split(addtime(dff), lag_range)
    if "xgb" in method:
        print("----- Working on 'xgb' method...")
        dtrain = xgb.DMatrix(x, label=y)
        dtest = xgb.DMatrix(xtest, label=ytest)
        time0 = time.time()
        bst = xgb.train({'max_depth':3, 'eta':0.3,
                         'booster':'gbtree'},
                        dtrain, 20)
        train_time = time.time() - time0
        confusion, test_time = Errmodel(bst,dtrain,dtrain.get_label(),dtest,dtest.get_label(),
                 ntree_limit=bst.best_iteration)
        print(confusion,'\n', train_time,'\n', test_time)
        importance = sorted(bst.get_score().items(), key=lambda x: x[1])
        result['xgb']= {'model': bst,
                'confusion': confusion,
                'train_time': train_time,
                'test_time': test_time,
                'importance': importance,
                'best_iter': bst.best_iteration}
        print("best_iter", bst.best_iteration)
    if "lr" in method:
        print("----- Working on 'lr' method...")
        lr = LinearRegression(fit_intercept=True, normalize=False, copy_X=False, n_jobs=-1)
        time0 = time.time()
        lr.fit(x, y)
        train_time = time.time() - time0
        confusion, test_time = Errmodel(lr,x,y,xtest,ytest)
        print(confusion,'\n', train_time,'\n', test_time)
        result['lr']= {'model': lr,
                'confusion':confusion,
                'train_time': train_time,
                'test_time': test_time}
    if "rf" in method:
        print("----- Working on 'rf' method...")
        rf = RandomForestRegressor(n_estimators=500, n_jobs=-1, verbose=0,
                                   max_features='log2')
        time0 = time.time()
        rf.fit(x, y)
        train_time = time.time() - time0
        confusion, test_time = Errmodel(rf,x,y,xtest,ytest)
        print(confusion,'\n', train_time,'\n', test_time)
        result['rf']= {'model': rf,
                'confusion':confusion,
                'train_time': train_time,
                'test_time': test_time}
    return result
    
def MCexperiment(df, method, *All_result, lag_range = range(96*1,96*22)):
    '''
    Do the Monto-calo experiment
    NOTE: the results are stored in the global variable "Allresult"
    '''
    # global All_result
    allvar = df.columns.to_native_types()
    cols = [col for col in allvar if col not in All_result[0]]
    n = len(cols)
    print(str(n)+" vars are left.")
    time0= time.time()
    random.shuffle(cols)
    i = 0
    for col in cols:
        print('\n-------------------------------')
        print('---------- On variable '+col)
        i += 1
        dff = df[[col]]
        All_result[0][col] = MakePrediction(dff, method, lag_range)
        print('---------- Done. '+str(i/n*100)+'% finished.')
        print('Time left: '+str((time.time()-time0)/i*(n-i)/60)+' min')

def result_table(result, tablename = None):
    '''
    read and convert info from the result of MCexperiment()
    '''
    if tablename is None: # find the table names automatically
        if sum(np.diff([len(result[col]) for col in result])!=0)>0:
            raise Exception("N of methods are not the same!")
        tablename = result[list(result.keys())[0]].keys()
    last = {}
    for tb in tablename:
        test_RMSE = [result[x][tb]['confusion']['test']['RMSE'] for x in result]
        test_MAPE = [result[x][tb]['confusion']['test']['MAPE'] for x in result]
        test_time = [result[x][tb]['test_time'] for x in result]
        test_n0s = [result[x][tb]['confusion']['test']['n_0s'] for x in result]
        train_time = [result[x][tb]['train_time'] for x in result]
        df = pd.DataFrame({"test_RMSE":test_RMSE,'test_MAPE':test_MAPE,
                           'test_time':test_time,'train_time':train_time,
                           'test_n0s': test_n0s})
        if tb=='xgb':
            df['best_iter']=[result[x][tb]['best_iter'] for x in result]
        last[tb] = df
    if 'xgb' in tablename:
        importance = {}
        for col in result:
            importance[col] = pd.DataFrame(result[col]['xgb']['importance'],
                        columns=['varname','score'])
            importance[col].index = importance[col].pop('varname')
        last['importance'] = importance
    return last

def analyze_result_table(tbs,points=96):
    '''
    dig out info from the result of result_table()
    '''
    last = {}
    for tb in tbs:
        if tb != 'importance':
            last[tb] = tbs[tb].describe()
        else: # to analyze the 'importance' table
            allcom = pd.concat(tbs[tb], axis=1)
            allcom = allcom.fillna(0)
            
            allcom = allcom.apply(np.sum, axis=1).sort_values(ascending=False)
            allcom = pd.DataFrame(allcom)
            ind = list(allcom.index)
            allcom['days'] = np.nan
            for item in ind:
                if item.startswith('lag'):
                    allcom['days'][item] = int(item[3:])/points
            last['importance'] = allcom
    return last

