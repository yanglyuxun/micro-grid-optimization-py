# 被trial3.py调用的R code
# 前面的部分是各种需要定义的函数，最后一部分是程序运行的主体，可从最后开始阅读

#%% 需要安装的package：
library(lubridate)
library(pso)

#%% 各种函数 -----------------

# 适应度函数（越小越好） - output is the cost+punish
# 德国模式：完全不允许电池、电网交互---------------
evaluate1 = function(Batrate){
  # 检查长度
  #if(length(Batrate)!=nx){message("Length Error!");return(NA)}
  # 计算Bat,sell
  Bat = (iny - outy) * Batrate
  Sell = iny - outy - Bat 
  Batreal = Bat # true electricity in the battery
  Batreal[Bat>0] = Bat[Bat>0] * batrateio # 充电效率 
  # 电池容量约束惩罚
  batnow = cumsum(Batreal) + 0.4*batmax
  pun = punish * (sum((batnow - batmax)[batnow > batmax]) +
                     -sum(batnow[batnow < 0]) +
                     if(0.4*batmax>batnow[nx]){(0.4*batmax-batnow[nx])*nx}else{0}+ #（最后电量保持80%以上)
                     sum((Bat - bat15max)[Bat > bat15max]) + # 电池充放电速率惩罚
                     sum((bat15min - Bat)[Bat < bat15min]))
  return( # cost = 
    -sum(ifelse(Sell>0, sellp, buyp) * Sell) +
      batdepre * sum(abs(Bat)) +
      pun
  )
}
PSOsolve1 = function(iny,outy, real_iny, real_outy){
  PSOresult = list(par=rep(1,nx))
  
  time0 = now()
  cost=c(evaluate1(rep(0.5,nx)))
  i = 1 # the n.row of cost
  while(i<2 || cost[i-1]-cost[i]>no_improvement){
    PSOresult = psoptim(PSOresult$par,evaluate1,lower = rep(0,nx),upper=rep(1,nx),
                        control = control)
    i=i+1
    cost[i] = evaluate1(PSOresult$par)
  }
  # calculate the real profit:
  Bat = (iny - outy) * PSOresult$par
  Batreal = Bat
  Batreal[Bat>0] = Bat[Bat>0] * batrateio # 充电效率 
  batnow = cumsum(Batreal) + 0.4*batmax
  if(sum((batnow>batmax) + (batnow<0)) >0 ){stop("Error1!")}
  return(list(time = as.duration(now()-time0), 
              realProfit = calc_profit1(real_iny,real_outy,batnow)))
}
calc_profit1= function(real_iny, real_outy, batnow){
  batnowreal = 0.4 * batmax
  Batreal = rep(NA, nx) 
  for(i in (1:nx)){
    IO = real_iny[i] - real_outy[i]
    target = batnow[i] - batnowreal
    if(IO * target <=0) {Batreal[i]=0}else{
      if(IO>0){
        Batreal[i] = min(target/batrateio, IO, (batmax - batnowreal)/batrateio, bat15max)
      }else{
        Batreal[i] = max(target, IO, (-batnowreal), bat15min)
      }
    }
    batnowreal = batnowreal + ifelse(Batreal[i]>0,Batreal[i]*batrateio, Batreal[i])
  }
  Sellreal = real_iny - real_outy - Batreal
  if(sum(Batreal * Sellreal<0)>0){stop("Error2!")}
  deltabatall = batnowreal - 0.4 * batmax
  return(
    sum(ifelse(Sellreal>0, sellp, buyp) * Sellreal) +
      -batdepre * sum(abs(Batreal))+
      subsidy_all + 
      ifelse(deltabatall<0, deltabatall*mean(buyp), deltabatall*mean(sellp))
  )
}
# 中间模式：允许电网向电池充电，不允许电池向电网卖电----------------
evaluate2 = function(Batrate_buyb){
  # 计算Bat,sell
  Batrate = Batrate_buyb[1:96]
  buyb = Batrate_buyb[97:192]
  Bat = (iny - outy) * Batrate
  Sell = iny - outy - Bat 
  batall = Bat+buyb
  batallreal = batall
  batallreal[batall>0] = batall[batall>0] * batrateio
  # 电池容量约束惩罚
  batnow = cumsum(batallreal) + 0.4*batmax
  pun = punish * (sum((batnow - batmax)[batnow > batmax]) +
                    -sum(batnow[batnow < 0]) +
                    if(0.4*batmax>batnow[nx]){(0.4*batmax-batnow[nx])*nx}else{0}+ #（最后电量保持80%以上)
                    sum((batall - bat15max)[batall > bat15max]) + # 电池充放电速率惩罚
                    sum((bat15min - batall)[bat15min>batall ]))
  return( # cost = 
    -sum(ifelse(Sell-buyb>0, sellp, buyp) * (Sell-buyb)) +
      batdepre * sum(abs(batall)) +
      pun
  )
}
PSOsolve2 = function(iny,outy, real_iny, real_outy){
  PSOresult = list(par=c(rep(0.5,nx),rep(0,nx)))
  time0 = now()
  cost=c(evaluate2(PSOresult$par))
  i = 1 # the n.row of cost
  while(i<2 || cost[i-1]-cost[i]>no_improvement){
    PSOresult = psoptim(PSOresult$par,evaluate2,
                        lower = c(rep(0,nx),rep(0,nx)), ####
                        upper= c(rep(1,nx),rep(bat15max,nx)), ####
                        control = control)
    i=i+1
    cost[i] = evaluate2(PSOresult$par)
  }
  # calculate the real profit:
  batrate = PSOresult$par[1:96]
  buyb = PSOresult$par[97:192]
 # buybreal = buyb * batrateio
  Bat = (iny - outy) * batrate
  #Batreal = Bat
  #Batreal[Bat>0] = Bat[Bat>0] * batrateio 
  batall = Bat+buyb
  batallreal = batall
  batallreal[batall>0] = batall[batall>0] * batrateio
  batnow = cumsum(batallreal) + 0.4*batmax
  if(sum((batnow>batmax) + (batnow<0)) >0 ){stop("Error1!")}
  return(list(time = as.duration(now()-time0), 
              realProfit = calc_profit2(real_iny,real_outy,batnow,buyb)))
}
calc_profit2= function(real_iny, real_outy, batnow,buybtar){
  batnowreal = 0.4 * batmax
  Batreal = rep(NA, nx) 
  buyb = rep(NA,nx)
  for(i in (1:nx)){
    IO = real_iny[i] - real_outy[i]
    target = batnow[i] - batnowreal
    if(IO>=0){
      if(target>=0){
        batall =  min(target/batrateio,  (batmax - batnowreal)/batrateio, bat15max)
        buyb[i] = min(max(buybtar[i],0),batall)
        Batreal[i] = min(batall-buyb[i], IO)
      }else{ #target<0
        Batreal[i] = 0
        buyb[i] = 0
      }
    }else{ #IO<0
      if(target<=0){
        Batreal[i] = max(target, IO, (-batnowreal), bat15min)
        buyb[i] = 0
      }else{ # target>0
        Batreal[i] = 0
        buyb[i] = min(target/batrateio, (batmax - batnowreal)/batrateio, bat15max)
      }
    }
    batall = Batreal[i] + buyb[i]
    batnowreal = batnowreal + ifelse(batall>0,batall*batrateio, batall)
  }
  Sellreal = real_iny - real_outy - Batreal
  if(sum(Batreal * Sellreal<0)>0){stop("Error2!")}
  batallall = Batreal+buyb
  sellallall = Sellreal - buyb
  deltabatall = batnowreal - 0.4 * batmax
  #temp <<- data.frame(buyb=buyb,buybtar=buybtar,minus=buyb-buybtar,batnow=batnow)
  return(
    sum(ifelse(sellallall>0, sellp, buyp) * sellallall) +
      -batdepre * sum(abs(batallall))+
      subsidy_all + 
      ifelse(deltabatall<0, deltabatall*mean(buyp), deltabatall*mean(sellp))
  )
}
# 国内模式：完全运行电网、电池交互-------------
evaluate3 = evaluate2

PSOsolve3 = function(iny,outy, real_iny, real_outy){
  PSOresult = list(par=c(rep(0.5,nx),rep(0,nx)))
  time0 = now()
  cost=c(evaluate3(PSOresult$par))
  i = 1 # the n.row of cost
  while(i<2 || cost[i-1]-cost[i]>no_improvement){
    PSOresult = psoptim(PSOresult$par,evaluate3,
                        lower = c(rep(0,nx),rep(bat15min,nx)), ####
                        upper= c(rep(1,nx),rep(bat15max,nx)), ####
                        control = control)
    i=i+1
    cost[i] = evaluate3(PSOresult$par)
  }
  # calculate the real profit:
  batrate = PSOresult$par[1:96]
  buyb = PSOresult$par[97:192]
  #buybreal = buyb
  #buybreal[buyb>0] = buyb[buyb>0] * batrateio
  Bat = (iny - outy) * batrate
  #Batreal = Bat
  #Batreal[Bat>0] = Bat[Bat>0] * batrateio 
  batall = Bat+buyb
  batallreal = batall
  batallreal[batall>0] = batall[batall>0] * batrateio
  batnow = cumsum(batallreal) + 0.4*batmax
  if(sum((batnow>batmax) + (batnow<0)) >0 ){stop("Error1!")}
  return(list(time = as.duration(now()-time0), 
              realProfit = calc_profit3(real_iny,real_outy,batnow,buyb)))
}
calc_profit3= function(real_iny, real_outy, batnow, buybtar){ #####USE temp to CHECKING THIS FUNCTION
  batnowreal = 0.4 * batmax
  Bat = rep(NA, nx) 
  buyb = rep(NA,nx)
  for(i in (1:nx)){
    IO = real_iny[i] - real_outy[i]
    target = batnow[i] - batnowreal
    if(IO>=0){
      if(target>=0){
        batall =  min(target/batrateio,  (batmax - batnowreal)/batrateio, bat15max)
        buyb[i] = min(max(buybtar[i],0),batall)
        Bat[i] = min(batall-buyb[i], IO)
      }else{ #target<0
        Bat[i] = 0
        buyb[i] = max(target, (-batnowreal), bat15min)
      }
    }else{ #IO<0
      if(target<=0){
        batall = max(target, (-batnowreal), bat15min)
        buyb[i] = max(min(buybtar[i],0), batall)
        Bat[i] =max(batall-buyb[i], IO)
      }else{ # target>0
        Bat[i] = 0
        buyb[i] = min(target/batrateio, (batmax - batnowreal)/batrateio, bat15max)
      }
    }
    batall = Bat[i] + buyb[i]
    batnowreal = batnowreal + ifelse(batall>0,batall*batrateio, batall)
  }
  Sell = real_iny - real_outy - Bat
  if(sum(Bat * Sell<0)>0){stop("Error2!")}
  batallall = Bat+buyb
  sellallall = Sell - buyb
  #temp <<- data.frame(buyb=buyb,buybtar=buybtar,minus=buyb-buybtar,batnow=batnow)
  deltabatall = batnowreal - 0.4 * batmax
  return(
    sum(ifelse(sellallall>0, sellp, buyp) * sellallall) +
      -batdepre * sum(abs(batallall))+
      subsidy_all + 
      ifelse(deltabatall<0, deltabatall*mean(buyp), deltabatall*mean(sellp))
  )
}

#################
# calculation for "all bat" strategy: 
# 电池优先策略
calc_profit_all_bat = function(real_iny, real_outy){
  batnowreal = 0.4 * batmax
  Batreal = rep(NA, nx) 
  for(i in (1:nx)){
    IO = real_iny[i] - real_outy[i]
      if(IO>0){
        Batreal[i] = min(IO, (batmax - batnowreal)/batrateio, bat15max)
      }else{
        Batreal[i] = max(IO, (-batnowreal), bat15min)
      }
    batnowreal = batnowreal + ifelse(Batreal[i]>0,Batreal[i]*batrateio, Batreal[i])
  }
  Sellreal = real_iny - real_outy - Batreal
  if(sum(Batreal * Sellreal<0)>0){stop("Error2!")}
  deltabatall = batnowreal - 0.4 * batmax
  return(
    sum(ifelse(Sellreal>0, sellp, buyp) * Sellreal) +
      -batdepre * sum(abs(Batreal))+
      subsidy_all +
      ifelse(deltabatall<0, deltabatall*mean(buyp), deltabatall*mean(sellp))
  )
}
# calculation for "no bat" strategy:
calc_profit_no_bat = function(real_iny, real_outy){
  Sellreal = real_iny - real_outy
  return(
    sum(ifelse(Sellreal>0, sellp, buyp) * Sellreal)+
      subsidy_all
  )
}
# calculation for top-bottom strategy:
# 削峰填谷策略
# (charge when price is low, and use bat when price is high)
calc_tp2 = function(real_iny,real_outy){
  batnowreal = 0.4 * batmax
  Batreal = rep(NA, nx) 
  buyb = rep(NA, nx) 
  for(i in 1:nx){
    IO = real_iny[i] - real_outy[i]
    if(IO>0){
      if(buyp[i]<mean(buyp)){
        buyb[i]= min((batmax - batnowreal[i])/batrateio, bat15max)
        Batreal[i]= 0
      }else{ # buyp[i]>mean(buyp)
        Batreal[i]=0
        buyb[i] = 0#max(-batnowreal[i], bat15min) #???
      }
    }else{ # IO<=0
      if(buyp[i]<mean(buyp)){
        Batreal[i]=0
        buyb[i] = min((batmax - batnowreal[i])/batrateio, bat15max)
      }else{ # buyp[i]>mean(buyp)
        batall = max(-batnowreal[i], bat15min)
        Batreal[i] = max(batall, IO)
        buyb[i] = 0#batall - Batreal[i] #???
      }
    }
    batall = Batreal[i] + buyb[i]
    batnowreal[i+1] = batnowreal[i] + ifelse(batall>0,batall*batrateio, batall)
  }
  Sellreal = real_iny - real_outy - Batreal
  if(sum(Batreal * Sellreal<0)>0){stop("Error2!")}
  batallall = Batreal + buyb
  sellallall = Sellreal - buyb
  #print(batnowreal)
  #print(sellallall)
  deltabatall = batnowreal[i+1]-0.4 * batmax
  return(
    sum(ifelse(sellallall>0, sellp, buyp) * sellallall) +
      -batdepre * sum(abs(batallall))+
      subsidy_all +
      ifelse(deltabatall<0, deltabatall*mean(buyp), deltabatall*mean(sellp))
  )
}
# 另一种削峰填谷策略（可忽略）
calc_tp3 = function(real_iny,real_outy){
  batnowreal = 0.4 * batmax
  Batreal = rep(NA, nx) 
  buyb = rep(NA, nx) 
  for(i in 1:nx){
    IO = real_iny[i] - real_outy[i]
    if(IO>0){
      if(buyp[i]<mean(buyp)){
        buyb[i]= min((batmax - batnowreal[i])/batrateio, bat15max)
        Batreal[i]= 0
      }else{ # buyp[i]>mean(buyp)
        Batreal[i]=0
        buyb[i] = max(-batnowreal[i], bat15min) #???
      }
    }else{ # IO<=0
      if(buyp[i]<mean(buyp)){
        Batreal[i]=0
        buyb[i] = min((batmax - batnowreal[i])/batrateio, bat15max)
      }else{ # buyp[i]>mean(buyp)
        batall = max(-batnowreal[i], bat15min)
        Batreal[i] = max(batall, IO)
        buyb[i] = batall - Batreal[i] #???
      }
    }
    batall = Batreal[i] + buyb[i]
    batnowreal[i+1] = batnowreal[i] + ifelse(batall>0,batall*batrateio, batall)
  }
  Sellreal = real_iny - real_outy - Batreal
  if(sum(Batreal * Sellreal<0)>0){stop("Error2!")}
  batallall = Batreal + buyb
  sellallall = Sellreal - buyb
  #print(batnowreal)
  #print(sellallall)
  deltabatall = batnowreal[i+1]-0.4 * batmax
  return(
    sum(ifelse(sellallall>0, sellp, buyp) * sellallall) +
      -batdepre * sum(abs(batallall))+
      subsidy_all +
      ifelse(deltabatall<0, deltabatall*mean(buyp), deltabatall*mean(sellp))
  )
}

#旧的参数目录（已废弃）：
#subsidy = 0.42 * (1-0.17) * (1-0.25) # 补贴，减去增值税和所得税
#buy = function(hour){
#  # 上海市电网夏季销售电价表（单一制分时电价用户）工商业及其他用电 http://www.sgcc.com.cn/dlfw/djzc/
#  return(ifelse(hour>=6 & hour<22, 1.044, 0.513))
#}
#sell = 0.85-subsidy # minus subsidy, because it always exists
#hours = sort(rep(0:23,4))
#buyp = buy(hours)
#sellp = rep(sell, 96)
#batmax = 50 # 电池容量(kwh) 
#bat15min = -0.2 * batmax /4 # 过量惩罚系数
#bat15max = 0.2 * batmax /4 # 每15分钟最大充电/放电值: 每小时充放电容量不能超过其最大容量的 20%
#batdepre = 0.3264/2# 电池折旧成本／（kW·h）#http://cn.trustexporter.com/cp-shanbaony/o4282865.htm
#batrateio = 0.85 # 电池充放电效率
#no_improvement = 1 # how much profit increase is condidered to be no improvement
#control = list(maxit = 3000, trace=F, REPORT=500) # the control for psoptim()


#%% main program 程序从这里执行-------------------------------------------------------------------------------------

source('R-para.R') # 从另一个文件（python写好的）读入各个参数
punish = 1e10 # 定义一个过量惩罚系数，用于保证PSO在限制条件内搜寻最优值

df = read.csv('out.csv') #读入数据
iny = df$iny
outy = df$outyp
real_iny = df$iny
real_outy = df$outytrue
subsidy_all = sum(real_iny) * subsidy
nx=length(iny)
result11 = PSOsolve1(iny,outy,real_iny,real_outy) #模式1
result21 = PSOsolve2(iny,outy,real_iny,real_outy) #模式2
result31 = PSOsolve3(iny,outy,real_iny,real_outy) #模式3
outy = real_outy # 将预测数据换成真实数据，影响后面的3个结果
result12 = PSOsolve1(iny,outy,real_iny,real_outy) #模式1
result22 = PSOsolve2(iny,outy,real_iny,real_outy) #模式2
result32 = PSOsolve3(iny,outy,real_iny,real_outy) #模式3
#calc_tp3(real_iny,real_outy) 
output = data.frame(time = as.numeric(c(result11$time,result12$time,
                                        result21$time,result22$time,
                                        result31$time,result32$time,0,0,0,0),'s'), 
           profit= c(result11$realProfit,
                     result12$realProfit,
                     result21$realProfit,
                     result22$realProfit,
                     result31$realProfit,
                     result32$realProfit,
                     calc_profit_all_bat(real_iny, real_outy), #电池优先策略
                     calc_profit_no_bat(real_iny, real_outy), #不用电池
                     calc_tp2(real_iny,real_outy),  #削峰填谷
                     calc_tp3(real_iny,real_outy))) 
write.csv(output, 'Routput.csv') #写入输出文件
