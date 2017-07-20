#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 12 15:53:39 2017

@author:
"""
#%%
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
#from pyswarm import pso

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

#%% new part
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
def predict_a_day(df,date,varns): #seperately!
    'predict point by point! More precise'
    prediction = df.loc[df.index.date==date].copy()
    time0 = 0
    for d in df.index[df.index.date==date]:
        prediction.loc[d], time1 = predict_a_point(df,d,varns)
        time0 += time1
        print(d)
    return prediction,time0

def predict_96_points(df,varns): # Together!
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


#%% PSO
# public variables:

#def buy(hour):
#    '上海市电网夏季销售电价表（单一制分时电价用户）工商业及其他用电 http://www.sgcc.com.cn/dlfw/djzc/'
#    return np.where((hour>=6) & (hour<22), 1.044,0.513)
#
#paras = {}
#subsidy = 0.42 * (1-0.17) * (1-0.25) # 补贴，减去增值税和所得税
#buyp = buy(df.index[:96].hour)
#sellp = np.array(0.85-subsidy).repeat(96) # minus subsidy because it always exists
#batmax = 10000 # 电池容量(kwh)
#punish = 1e10 # 过量惩罚系数
#bat15min = -0.2 * batmax /4 # 过量惩罚系数
#bat15max = 0.2 * batmax /4 # 每15分钟最大充电/放电值: 每小时充放电容量不能超过其最大容量的 20%
#batdepre = 0.1# 电池折旧成本／（kW·h）
#batrateio = 0.8 # 电池充放电效率
#no_improvement = 2 # how much profit increase is condidered to be no improvement
##'control' : list(maxit = 3000, trace=T, REPORT=500) # the control for psoptim()
#iny = iny.T.as_matrix()[0][:96]
#outy = outytrue.T.as_matrix()[0]
#
#def evaluate(Batrate):
#  # 检查长度
#  #if(length(Batrate)!=nx){message("Length Error!");return(NA)}
#  # 计算Bat,sell
#  Bat = (iny - outy) * Batrate
#  Sell = iny - outy - Bat 
#  return -np.sum(np.where(Sell>0, sellp, buyp) * Sell) + batdepre * np.sum(np.abs(Bat))
#
#
#def con(Batrate):
#  Bat = (iny - outy) * Batrate
#  Sell = iny - outy - Bat 
#  Batreal = Bat # true electricity in the battery
#  Batreal[Bat>0] = Bat[Bat>0] * batrateio # 充电效率 
#  # 电池容量约束惩罚
#  batnow = np.cumsum(Batreal) + 0.8*batmax
#  if 0.8*batmax>batnow[-1]:
#      temp = (0.8*batmax-batnow[-1])*len(batnow)
#  else:
#      temp = 0
#  pun = punish * (np.sum((batnow - batmax)[batnow > batmax]) +
#                     -np.sum(batnow[batnow < 0]) +
#                     temp+ #（最后电量保持80%以上)
#                     np.sum((Bat - bat15max)[Bat > bat15max]) + # 电池充放电速率惩罚
#                     np.sum((bat15min - Bat)[Bat < bat15min]))
#  return [-pun]
#
#lb = np.zeros(96)
#ub = np.ones(96)
#
#xopt, fopt = pso(evaluate, lb, ub, ieqcons=[], f_ieqcons=con, args=(), kwargs={},
#    swarmsize=100, omega=0.5, phip=0.5, phig=0.5, maxiter=1000, minstep=1e-8,
#    minfunc=1e-8, debug=False)
#
## Optimum should be around x=[0.5, 0.76] with banana(x)=4.5 and con(x)=0

##%% R - PSO
## import rpy2's package module
#import rpy2.robjects.packages as rpackages
## import R's utility package
#utils = rpackages.importr('utils')
## select a mirror for R packages
#utils.chooseCRANmirror(ind=33) # select the first mirror in the list
## R package names
#packnames = ['lubridate','tidyverse','RSNNS','pso']
## R vector of strings
#from rpy2.robjects.vectors import StrVector
## Selectively install what needs to be install.
## We are fancy, just because we can.
#names_to_install = [x for x in packnames if not rpackages.isinstalled(x)]
#if len(names_to_install) > 0:
#    utils.install_packages(StrVector(names_to_install))
#
## source file
#import rpy2.robjects as robjects
#robjects.r.source('trial5-fun.R')

#%% MC

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
    inyorigin,outyall,outdate = select_1d(dfin,df)
    iny = outyall.loc[outyall.index.date==outdate].copy() #only get the index
    for i in iny.index:
        t1 = np.array([i.hour*60**2+i.minute*60+i.second for i in inyorigin.index.time])
        t2 = i.hour*60**2+i.minute*60+i.second
        iny.loc[i].iloc[0] = inyorigin.iloc[np.argmin(np.abs(t1-t2))].values
    print('Predicting...')
    pred,time0 = predict_96_points(outyall,varns)
    outytrue = outyall.loc[outyall.index.date==outdate].copy()
    outyp = outytrue.copy()
    outyp.iloc[:] = pred[:,np.newaxis]
    # tuning the data
    # iny = iny 
    print('Output...')
    output = pd.concat([iny,outyp,outytrue],axis=1)
    output.columns = ['iny','outyp','outytrue']
    output.to_csv('out.csv',index=False)
    print('PSO...') # use R script to do the PSO part
    if os.path.exists('Routput.csv'):
        os.remove('Routput.csv')
    os.system('Rscript ./trial3-R.R')
    Rout = pd.read_csv('Routput.csv')
    Rdf = pd.DataFrame({'pre_t':Rout['time'][0],'real_t':Rout['time'][1],
     'pre_profit':Rout['profit'][0],'real_profit':Rout['profit'][1],
     'all_bat_p':Rout['profit'][2],'no_bat_p':Rout['profit'][3]},index=[outdate])
    os.remove('out.csv')
    os.remove('Routput.csv')
    return Rdf

def MC_multi(dfin,df,varns,*result,rep=10):
    for i in range(rep):
        result[0].append(MC_select_and_exp(dfin,df,varns))
        print('================================')
        print(len(result[0]))
        print('================================')

def find_multi_index(dfin,df,rep=100):
    sumin=0
    sumout=0
    for j in range(rep):
        inyorigin,outyall,outdate = select_1d(dfin,df)
        iny = outyall.loc[outyall.index.date==outdate].copy()
        for i in iny.index:
            t1 = np.array([i.hour*60**2+i.minute*60+i.second for i in inyorigin.index.time])
            t2 = i.hour*60**2+i.minute*60+i.second
            iny.loc[i].iloc[0] = inyorigin.iloc[np.argmin(np.abs(t1-t2))].values
        outy = outyall.loc[outyall.index.date==outdate].copy()
        sumin+=np.sum(iny.as_matrix())
        sumout+=np.sum(outy.as_matrix())
        print(j)
    return sumout/sumin