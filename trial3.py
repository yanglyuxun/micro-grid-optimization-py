#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This is used to do the PSO MC experiment
包括预测和微电网调度

"""
#%% import
import pandas as pd
import numpy as np
import pickle
#import datetime
#import time
# import datetime as dt
from trial3_fun import * # the functions file

#%% 负荷数据和预处理
df = pd.read_csv("LD2011_2014.txt",sep=";",decimal=',',
                 header=0,engine='c', index_col = 0,
                 verbose=False)
df.index = pd.to_datetime(df.index)

zero_to_na(df) # 根据原数据说明，数据开头的大量0应该是NaN（inplace）

sorted(df.apply(lambda x: x.count()), reverse=1) # 看看每列还剩下多少有效数据
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

# is there too many 0s? 太多0的列不要
sorted(df.apply(lambda x: (x==0).sum()))
# drop the ones with 0s more than 1000
df = df.drop(df.columns[df.apply(lambda x: (x==0).sum()>1000)], axis=1)
# drop the first and the last date because of imcompletion
df = drop_dates(df)

# 选取一个合适的用电单位 -- 用于构造一个8kw的用电场景
df_des = df.describe().T
df_out = df[['MT_019']]
df_out.describe()
# mean p=8kw

df_out/=4 # convert kw -> kwh/15min 因为后面模型里面算的是每15min用了几度电，所以要除以4

#%% 发电数据和预处理 ----------------
#cols = ["zd","t"]
#names = ["time","y"]
dfin = read_all_data('./Data/')

# 读入之前的重要变量名的文件 --> 转换成数字
with open('vars.txt') as f:
    varns = [int(i.strip().replace('lag','')) for i in f.readlines()]

# 选一个发电单位 --- 构造50kw的发电场景
pd.concat([i.describe().T for i in dfin])
df_in = dfin[5]
df_in /= 100
df_in.describe()
# max = 52.8kw

df_in/=4 # convert kw -> kwh/15min 同样，因为后面需要的是每15min发了几度电
#%% 比较2种预测方法的结果不同
MC_compare_result = MC_compare_2_prediction(dfin,df,varns,rep=30)
MC_compare_result.describe()
#最后选用xgboost

#%% PSO 模拟实验
# 这里定义了一个类Experiment(),方便进行不同的实验
# 初始化：给进之前选好的发电数据（df_in），用电数据（df_out），和读入的重要变量的滞后期（varns）（用于简化xgboost模型）
Ex = Experiment(df_in,df_out,varns)
# Ex = Experiment(df_in,df_out,varns,result=pso_results) #如果想继续之前的实验，可以用这个来初始化，这样之前的结果pso_results就进去了
# 设置参数：
Ex.set_para(
        subsidy=0.42 * (1-0.17) * (1-0.25), # 新能源补贴，减去增值税和所得税
        buy_hour_price={(0,6):0.513,(6,22):1.044,(22,24):0.513}, # 买电的分时电价：0-6时0.513元，6-22时1.044元，等。开头需要是0时，结尾需要是24时，中间不能有空缺。
        sell_price=0.85, #卖电的电价
        batmax=50, #电池容量(kwh) 
        bat_rate=0.2 , #每小时充放电容量不能超过其最大容量的 20%
        batdepre=0.3264,# 电池折旧成本／（kW·h），每充放电1度，折旧0.3264元。参考：http://cn.trustexporter.com/cp-shanbaony/o4282865.htm
        batrateio=0.85, # 电池充放电效率。充1度电，只能放出0.85度。
        no_improvement=1, # 这个参数是用来终止pso计算的。程序中pso会反复反复的算，直到两次计算（中间隔了3000（下面的maxit）次种群刷新）的目标值差异在这个值以下，停止计算（但并不一定就离真实最优值只有这个距离了）。如果设大了，可能会达不到近似最优值；如果设小了，可能耗时非常长。
        maxit=3000 # 每一块计算的pso种群刷新循环数。如果设小了，就很容易达到上面的no_improvement而提前停止，如果设大了，即使到达了最优值还要计算很久。
        )
# 开始实验：rep是重复实验的次数。
Ex.MC_multi(rep=1000)
# 每一次实验，都会从df_in抽一天，df_out抽1天（包括真实值和预测值），然后传递到R里面进行预测，再把结果保存起来。
# 注意：这里需要和R进行交互，需要安装好R以及2个package：lubridate和pso。确保shell命令Rscript可用。原因是在python中没有找到比较方便的pso的包。
# 交互方式：尝试过Rpy2的包，但是各种出错，所以直接用文件读写的方式。它会先把数据存入out.csv，把参数写入R-para.R，然后shell执行'Rscript trual3-R.R'命令，会执行R程序调用out.csv和R-para.R，进行所有pso运算，结果存入Routput.csv，然后它再读入Routput.csv，保存到Ex.result中去。这一部分见trial3_fun.py的line356-377.
# 注意：这种交互方式不能获取R返回的错误值，所以如果出错的话不会显示，不会停止，但是Ex.result的该项会是'error'。如果发现运行太快而且Ex.result全都是‘error’，那应该是R没装好或者包没有装好。
# 这个命令可以随时终止，下次继续执行的时候它会继续在Ex.result里面添加项目。（这样就可以不一次跑完几百次了，但是要记得保存好Ex这个object以便下次接着跑。实际上它有自动保存的功能：）
# 注意：它有自动保存到文件功能：每得出1个结果，它都会自动用pickle把整个Ex保存到'save.pickle'文件。如果ipython挂了，辛辛苦苦跑的结果还在。用下面的命令取回保存的结果：
#    with open('save.pickle','rb') as f:
#        Ex = pickle.load(f)
# Ex.result是一个list，每一项是一个实验结果。使用下面的命令得到所有结果的统计信息：
pso_results_stat = Ex.stat() # 得到一个统计表，数值是相应的利润值的各种统计量. 利润值是平均每日的利润值，=卖电收入+新能源补贴-买电成本-电池折旧，没有考虑其他，根据发电、用电之间的大小关系可能出现正值或者负值。
# index说明：
#all_bat_p：电池优先策略
#cut_top2：削峰填谷策略
#cut_top3：削峰填谷策略变体（可忽略）
#no_bat_p：不用电池
#pre_profit1：用预测值时，模式1（不允许电池从电网买卖电）
#pre_profit2：用预测值时，模式2（仅允许电池从电网买电）
#pre_profit3：用预测值时，模式3（允许电池和电网随意交互）
#pre_t1：算用预测值，模式1的耗时（s）
#pre_t2：算用预测值，模式2的耗时（s）
#pre_t3：算用预测值，模式3的耗时（s）
#real_profit1：用真实值时，模式1（不允许电池从电网买卖电）
#real_profit2：用真实值时，模式2（仅允许电池从电网买电）
#real_profit3：用真实值时，模式3（允许电池和电网随意交互）
#real_t1：算用真实值，模式1的耗时（s）
#real_t2：算用真实值，模式2的耗时（s）
#real_t3：算用真实值，模式3的耗时（s）

