#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
The functions for "trial1.py"
Created on Fri Jun 30 16:46:49 2017
@author: Lyuxun Yang
"""

import pandas as pd
import numpy as np
import datetime as dt

def addtime(df):
  df["date"] = [x.date() for x in df.time]
  df["month"] = [x.month for x in df.time]
  df["day"] = [x.day for x in df.time]
  df["daypart"] = np.where(df.day<=10, 1, np.where(df.day<=20, 2, 3))
  df["weekdays"] = [x.weekday() for x in df.time]
  df["hour"] = [x.hour for x in df.time]
  df["minute"] = [x.minute for x in df.time]
  return(df)

def addlag_split(df, step_range): # step_range = range(96,96*22)
    """
    Add lag vars in the data and split it into train set and test set
    """
    MCdfselect = df[["time", "iny", "outy", "day", "weekdays", "hour", "minute"]].copy()
    MCdfselect = pd.get_dummies(MCdfselect, 
                                columns=["day","weekdays","hour","minute"])
    #n = step_range[-1]
    dflist = [MCdfselect]
    for i in step_range:
        dflist.append(MCdfselect.outy.shift(i).rename("lag" + str(i)))
    MCdfselect = pd.concat(dflist,axis=1)
    MCdfselect = MCdfselect.dropna()
    traindf = MCdfselect.loc[MCdfselect.time < dt.date(2016,8,1),:]
    testdf = MCdfselect.loc[MCdfselect.time >= dt.date(2016,8,1),:]
    x = traindf.drop(["iny","time","outy"],axis=1)
    y = traindf.outy
    xtest = testdf.drop(["iny","time","outy"],axis=1)
    ytest = testdf.outy
    return x,y,xtest,ytest

def Err(prediction, truevalue):
    RMSE = np.sqrt( ((prediction - truevalue)**2).mean() )
    if (sum(truevalue == 0))>0:
        print("There are some 0 values!")
        isnotzero = truevalue != 0
        prediction, truevalue = prediction[isnotzero], truevalue[isnotzero]
    MAPE = (np.abs(prediction - truevalue) / truevalue).mean()
    return [RMSE, MAPE]
def Errmodel(model,x,y,xtest,ytest):
    yp = model.predict(x)
    ytestp = model.predict(xtest)
    return pd.DataFrame({"train":Err(yp,y),
                         "test":Err(ytestp,ytest)}, index=["RMSE","MAPE"])


