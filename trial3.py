#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This is used to do the PSO MC experiment
Created on Wed Jul 12 15:52:25 2017

@author:
"""
import pandas as pd
import numpy as np
import pickle
#import datetime
#import time
# import datetime as dt
from trial3_fun import * # the functions file

# data preparing -- load ----------------
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
# drop the first and the last date because of imcompletion
df = drop_dates(df)

# data preparing -- in ----------------
#cols = ["zd","t"]
#names = ["time","y"]
dfin = read_all_data('./Data/')

# These are the important variables --> to the lag numbers
with open('vars.txt') as f:
    varns = [int(i.strip().replace('lag','')) for i in f.readlines()]

MC_compare_result = MC_compare_2_prediction(dfin,df,varns,rep=30)
MC_compare_result.describe()
# PSO 

#find_multi_index(dfin,df,rep=1000) # tuning index: around 1
# (to let sum(iny)~=sum(outy))
# pso_results=[]
MC_multi(dfin,df,varns,pso_results,rep=1000)
with open('save.pickle','rb') as f:
    get_save = pickle.load(f)
pso_results_stat = result_stat(pso_results)
