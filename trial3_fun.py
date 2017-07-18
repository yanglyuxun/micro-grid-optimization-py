#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 12 15:53:39 2017

@author:
"""
import os
import pandas as pd
import numpy as np
import datetime
import time
#import random
import xgboost as xgb
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
import matplotlib.pyplot as plt

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



def Err(prediction, truevalue):
    '''
    calculate the RMSE, MAPE for some prediction
    '''
    prediction = np.array(prediction)
    truevalue = np.array(truevalue)
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

def read_all_data(dirs):
    dt = []
    files = os.listdir(dirs)
    for f in files:
        dfin = pd.read_csv(dirs+f)
        dfin.index = [datetime.datetime.fromtimestamp(i) for i in dfin.zd]
        dfin = dfin[['t']]
        dfin.columns=['in']
        dt.append(dfin)
        print(f)
    return dt

def drop_dates(df):
    drl= df.index.date[[1,-1]]
    df = df.loc[df.index.date != drl[0]]
    df = df.loc[df.index.date != drl[1]]
    return df

def select_1d(dfin,df):
    # train data has at least 30 days
    mindate = df.index.min() + pd.Timedelta(days=30)
    choi1 = pd.to_datetime(np.random.choice(df.index[df.index>mindate])).date()
    choi2 = np.random.choice(df.columns)
    dfnew = df.loc[df.index.date<=choi1, [choi2]].copy()
    in1 = np.random.randint(0,len(dfin))
    choi = pd.to_datetime(np.random.choice(dfin[in1].index)).date()
    dfinnew = dfin[in1].loc[dfin[in1].index.date==choi, ['in']].copy()
    return dfinnew,dfnew,choi1

def addlag_split(df, step_range, testdate, varns,ntrainmax=6000): # step_range = range(96,96*22)
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
        if i in varns:
            dflist.append(MCdfselect.outy.shift(i).rename("lag" + str(i)))
    MCdfselect = pd.concat(dflist,axis=1)
    MCdfselect = MCdfselect.dropna().sort_index()
    testind = MCdfselect.index.date == testdate
    traindf = MCdfselect.loc[~testind]
    testdf = MCdfselect.loc[testind]
    x = traindf.drop(["outy"],axis=1)
    y = traindf.outy
    xtest = testdf.drop(["outy"],axis=1)
    ytest = testdf.outy
#    x.values.astype('float')
#    y.values.astype('float')
#    xtest.values.astype('float')
#    ytest.values.astype('float')
    return x.iloc[-ntrainmax:],y.iloc[-ntrainmax:],xtest,ytest

def predict_a_point(df,datet,varns): 
    date0 = datet.date()
    dfnew = df.loc[(df.index.date!=date0) | (df.index==datet)]
    step0 = int((dfnew.index[-1]-dfnew.index[-2]) / (dfnew.index[-2]-dfnew.index[-3]))
    #print(step0)
    x,y,xtest,ytest = addlag_split(addtime(df), range(step0,96*22+step0), date0, varns)
    dtrain = xgb.DMatrix(x, label=y)
    dtest = xgb.DMatrix(xtest.loc[[datet]], label=ytest.loc[[datet]])
    time0 = time.time()####
    bst = xgb.train({'max_depth':3, 'eta':0.3,
                     'booster':'gbtree'},
                    dtrain, 20)
    result = bst.predict(dtest, ntree_limit=bst.best_iteration)
    use_time = time.time() - time0####
    return result[0],use_time
def predict_a_day(df,date,varns):
    'predict point by point! More precise'
    prediction = df.loc[df.index.date==date].copy()
    time0 = 0
    for d in df.index[df.index.date==date]:
        prediction.loc[d], time1 = predict_a_point(df,d,varns)
        time0 += time1
        print(d)
    return prediction,time0

def predict_96_points(df,varns): #### error is too large????????
    'predict together!'
    date0 = df.index[-1].date()
    x,y,xtest,ytest = addlag_split(addtime(df), range(96*1,96*22), date0,varns)
    dtrain = xgb.DMatrix(x, label=y)
    dtest = xgb.DMatrix(xtest, label=ytest)
    time0 = time.time()####
    bst = xgb.train({'max_depth':3, 'eta':0.3,
                     'booster':'gbtree'},
                    dtrain, 20)
    result = bst.predict(dtest, ntree_limit=bst.best_iteration)
    use_time = time.time() - time0####
    return result,use_time

def MC_compare_2_prediction(dfin,df,varns,rep=30,plot=4):
    'compare the 2 prediction methods'
    seperate = {'err':[],'time':[]}
    together = {'err':[],'time':[]}
    for i in range(rep):
        iny,outyall,outdate = select_1d(dfin,df)
        pred,time0 = predict_a_day(outyall,outdate,varns)
        seperate['err'].append(Err(pred,outyall.loc[outyall.index.date==outdate])[1])
        seperate['time'].append(time0)
        if plot>0:
            plt.figure()
            plt.plot(outyall.loc[outyall.index.date==outdate],'-b')
            plt.plot(pred,':r')
            plt.title('Seperate '+str(outdate))
        pred,time0 = predict_96_points(outyall,varns)#[-2:]
        together['err'].append(Err(pred,outyall.loc[outyall.index.date==outdate].T.as_matrix()[0])[1])
        together['time'].append(time0)
        if plot>0:
            plt.figure()
            plt.plot(outyall.loc[outyall.index.date==outdate].T.as_matrix()[0],'-b')
            plt.plot(pred,':r')
            plt.title('Together '+str(outdate))
            plot-=1
        print((i+1)/rep)
    return pd.DataFrame({'sep_Err':seperate['err'],'tog_Err':together['err'],
                         'sep_t':seperate['time'],'tog_t':together['time']})


def MC_select_and_exp(dfin,df,varns):
    print('Making data...')
    iny,outyall,outdate = select_1d(dfin,df)
    print('Data preprocessing...')
    #addlag_split(outyall, step_range, outdate)
    temp = predict_96_points(outyall,varns)
    
    
    