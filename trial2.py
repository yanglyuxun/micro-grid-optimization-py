#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
用所有用电单位的数据测试预测精度

"""

import pandas as pd
import numpy as np
# import datetime as dt

from trial2_fun import * # 调用写好的所有函数。凡是spyder自动打感叹号的函数，都是出自这里面
#run trial2_fun.py

# 数据预处理 ------------------
df = pd.read_csv("LD2011_2014.txt",sep=";",decimal=',',
                 header=0,engine='c', index_col = 0,
                 verbose=False)
df.index = pd.to_datetime(df.index) #转换时间格式

zero_to_na(df) # 根据原数据说明，数据开头的大量0应该是NaN（inplace）

sorted(df.apply(lambda x: x.count()), reverse=1) # 看看每列还剩下多少有效数据
#[16032,
# 28160,
# 28992,
# 30432,
# 30432,
# 31008,
# ...

# choose 28160 and drop columns 砍掉NAN太多的列
np.where(df.apply(lambda x: x.count()) <28160)
df = df.iloc[-28160:,]
df = df.dropna(axis=1)

# is there too many 0s? 砍掉0太多的列（说明这个单位用电太少了）
sorted(df.apply(lambda x: (x==0).sum()))
# drop the ones with 0s more than 1000
df = df.drop(df.columns[df.apply(lambda x: (x==0).sum()>1000)], axis=1)

# 重复实验 ---------------------
# All_result = {}  # 注意：在首次运行时需要运行本行，初始化。但中断后重新运行就不需要了。下面有解释。
MCexperiment(df, ['xgb','lr'], All_result, lag_range=range(96*1,96*22))
# 解释：df是数据dataframe
#       ['xgb','lr']表示对比xgboost和线性回归两种模型，也就是说每次抽好数据以后就分别用这两个模型预测和算误差。这个list有三个选项，分别是['xgb','lr','rf']，但是'rf'效果很差，就没浪费时间比较它了。
#        All_result 这个是用来存放结果的。函数在运行的时候会不断把结果往这个里面放。我没有用返回值的原因是：这个命令做下来需要很长时间，中间可能会强行终止，所以我需要每次产生一个结果就保存下来，即使中断运行，已有的结果已经在All_result里面了。再次运行这行命令时，它会自动检查All_result已经有哪些结果，然后接着跑剩下的数据。
#       因此，这样可以随时Ctrl-C终止运行，只要保存好All_result，下次直接接着运行即可。
#       lag_range=range(96*1,96*22)里面有2个数，第一个是输入的最后一个点到需要预测的那个点的步数距离，第二个是输入的第一个点到需要预测的哪个点的步数距离。换句话说就是滞后阶数的范围。由于1天有96个点，这里的意思就是我要输入现在以及之前的22天的所有点的数据，预测一天以后的1个点数据。
# 实验结果需要用下面的函数统计成可以看的形式：
Table_results = result_table(All_result)
table_ana = analyze_result_table(Table_results)
# 得到的table_ana里面有3项：
#   importance是各个变量的重要性从大到小的排名。加总了所有的实验结果。其中index中的lag多少表示滞后多少步，可以参考days中的数表示滞后了多少天（除了96）。其他的变量名表示相应的时间变量的哑变量，比如day_15表示‘是否是15号’这个哑变量。
#   lr和xgb本别是线性回归和xgboost的实验结果统计表。表中有test的MAPE平均误差百分比，RMSE均方跟误差等，time是耗时，n0s是看数据质量的（有多少0），可以不管。

# 下面这些和上面的这个实验类似，就是滞后期的范围不一样，表示预测未来多远的一个点不一样：
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

# 调取所有实验结果中的test_MAPE的中位数
[round(i['lr']['test_MAPE']['50%'],5) for i in [table_1,table_4,table_16,table_ana, table_962,table_964]]
[round(i['xgb']['test_MAPE']['50%'],5) for i in [table_1,table_4,table_16,table_ana, table_962,table_964]]
# 调取所有实验结果中的test_MAPE的标准差
[round(i['lr']['test_MAPE']['std'],4) for i in [table_1,table_4,table_16,table_ana, table_962,table_964]]
[round(i['xgb']['test_MAPE']['std'],4) for i in [table_1,table_4,table_16,table_ana, table_962,table_964]]


### 分析最重要的滞后变量，把变量名写到文件里面，在微电网调度中会用到（用来简化模型）
importance = table_1['importance']
len(importance.index[importance[0]>=20])
with open('vars.txt','w') as f:
    f.writelines([i+'\n' for i in importance.index[importance[0]>=20] if i.startswith('lag')])
