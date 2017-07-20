# These are the functions for "trial5.R"
library(lubridate)
library(pso)

## PSO -----------------

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
  batnow = cumsum(Batreal) + 0.8*batmax
  pun = punish * (sum((batnow - batmax)[batnow > batmax]) +
                     -sum(batnow[batnow < 0]) +
                     if(0.8*batmax>batnow[nx]){(0.8*batmax-batnow[nx])*nx}else{0}+ #（最后电量保持80%以上)
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
  subsidy_all <<- sum(real_iny) * subsidy
  time0 = now()
  cost=c(evaluate1(rep(0.5,nx)))
  i = 1 # the n.row of cost
  while(i<2 || cost[i-1]-cost[i]>no_improvement){
    PSOresult = psoptim(PSOresult$par,evaluate1,lower = rep(0,nx),upper=rep(1,nx),
                        control = control)
    i=i+1
    cost[i] = evaluate(PSOresult$par)
  }
  # calculate the real profit:
  Bat = (iny - outy) * PSOresult$par
  Batreal = Bat
  Batreal[Bat>0] = Bat[Bat>0] * batrateio # 充电效率 
  batnow = cumsum(Batreal) + 0.8*batmax
  if(sum((batnow>batmax) + (batnow<0)) >0 ){stop("Error1!")}
  return(list(time = as.duration(now()-time0), 
              realProfit = calc_profit1(real_iny,real_outy,batnow)))
}
calc_profit1= function(real_iny, real_outy, batnow){
  batnowreal = 0.8 * batmax
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
  return(
    sum(ifelse(Sellreal>0, sellp, buyp) * Sellreal) +
      -batdepre * sum(abs(Batreal))+
      subsidy_all + 
      ifelse(sum(Batreal)<0, sum(Batreal)*mean(buyp), sum(Batreal)*mean(sellp))
  )
}
# 中间模式：允许电网向电池充电，不允许电池向电网卖电----------------
evaluate2 = function(Batrate_buyb){
  # 检查长度
  #if(length(Batrate)!=nx){message("Length Error!");return(NA)}
  # 计算Bat,sell
  Batrate = Batrate_buy[1:96]
  buyb = Batrate_buy[97:192]
  buybreal = buyb * batrateio
  Bat = (iny - outy) * Batrate
  Sell = iny - outy - Bat 
  Batreal = Bat # true electricity in the battery
  Batreal[Bat>0] = Bat[Bat>0] * batrateio # 充电效率 
  # 电池容量约束惩罚
  batnow = cumsum(Batreal+buybreal) + 0.8*batmax
  pun = punish * (sum((batnow - batmax)[batnow > batmax]) +
                    -sum(batnow[batnow < 0]) +
                    if(0.8*batmax>batnow[nx]){(0.8*batmax-batnow[nx])*nx}else{0}+ #（最后电量保持80%以上)
                    sum((Bat+buyb - bat15max)[Bat+buyb > bat15max]) + # 电池充放电速率惩罚
                    sum((bat15min - (Bat+buyb))[(Bat+buyb) < bat15min]))
  return( # cost = 
    -sum(ifelse(Sell-buyb>0, sellp, buyp) * (Sell-buyb)) +
      batdepre * sum(abs(Bat+buyb)) +
      pun
  )
}
PSOsolve2 = function(iny,outy, real_iny, real_outy){
  PSOresult = list(par=c(rep(0.5,nx),rep(0,nx)))
  subsidy_all <<- sum(real_iny) * subsidy
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
  buybreal = buyb * batrateio
  Bat = (iny - outy) * batrate
  Batreal = Bat
  Batreal[Bat>0] = Bat[Bat>0] * batrateio 
  batnow = cumsum(Batreal+buybreal) + 0.8*batmax
  if(sum((batnow>batmax) + (batnow<0)) >0 ){stop("Error1!")}
  return(list(time = as.duration(now()-time0), 
              realProfit = calc_profit2(real_iny,real_outy,batnow)))
}
calc_profit2= function(real_iny, real_outy, batnow){
  batnowreal = 0.8 * batmax
  Batreal = rep(NA, nx) 
  buyb = rep(NA,nx)
  for(i in (1:nx)){
    IO = real_iny[i] - real_outy[i]
    target = batnow[i] - batnowreal
    if(IO>=0){
      if(target>=0){
        batall =  min(target/batrateio,  (batmax - batnowreal)/batrateio, bat15max)
        Batreal[i] = min(batall, IO)
        buyb[i] = batall-Batreal[i]
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
        buyb[i] = min(target/batrateio, (batmax - batnowreal)/batrateio, bat15min)
      }
    }
    batall = Batreal[i] + buyb[i]
    batnowreal = batnowreal + ifelse(batall>0,batall*batrateio, batall)
  }
  Sellreal = real_iny - real_outy - Batreal
  if(sum(Batreal * Sellreal<0)>0){stop("Error2!")}
  batallall = Batreal+buyb
  return(
    sum(ifelse(Sellreal>0, sellp, buyp) * Sellreal) +
      -batdepre * sum(abs(batallall))+
      subsidy_all + 
      ifelse(sum(batallall)<0, sum(batallall)*mean(buyp), sum(batallall)*mean(sellp))
  )
}
# 国内模式：完全运行电网、电池交互-------------
evaluate3 = function(Batrate_buyb){
  # 计算Bat,sell
  Batrate = Batrate_buy[1:96]
  buyb = Batrate_buy[97:192]
  buybreal = buyb
  buybreal[buyb>0] = buyb[buyb>0] * batrateio
  Bat = (iny - outy) * Batrate
  Sell = iny - outy - Bat 
  Batreal = Bat # true electricity in the battery
  Batreal[Bat>0] = Bat[Bat>0] * batrateio # 充电效率 
  # 电池容量约束惩罚
  batnow = cumsum(Batreal+buybreal) + 0.8*batmax
  pun = punish * (sum((batnow - batmax)[batnow > batmax]) +
                    -sum(batnow[batnow < 0]) +
                    if(0.8*batmax>batnow[nx]){(0.8*batmax-batnow[nx])*nx}else{0}+ #（最后电量保持80%以上)
                    sum((Bat+buyb - bat15max)[Bat+buyb > bat15max]) + # 电池充放电速率惩罚
                    sum((bat15min - (Bat+buyb))[bat15min>(Bat+buyb) ]))
  return( # cost = 
    -sum(ifelse(Sell-buyb>0, sellp, buyp) * (Sell-buyb)) +
      batdepre * sum(abs(Bat+buyb)) +
      pun
  )
}
########################## stop here
PSOsolve3 = function(iny,outy, real_iny, real_outy){
  PSOresult = list(par=c(rep(0.5,nx),rep(0,nx)))
  subsidy_all <<- sum(real_iny) * subsidy
  time0 = now()
  cost=c(evaluate3(PSOresult$par))
  i = 1 # the n.row of cost
  while(i<2 || cost[i-1]-cost[i]>no_improvement){
    PSOresult = psoptim(PSOresult$par,evaluate3,
                        lower = c(rep(0,nx),rep(0,nx)), ####
                        upper= c(rep(1,nx),rep(bat15max,nx)), ####
                        control = control)
    i=i+1
    cost[i] = evaluate3(PSOresult$par)
  }
  # calculate the real profit:
  batrate = PSOresult$par[1:96]
  buyb = PSOresult$par[97:192]
  buybreal = buyb * batrateio
  Bat = (iny - outy) * batrate
  Batreal = Bat
  Batreal[Bat>0] = Bat[Bat>0] * batrateio 
  batnow = cumsum(Batreal+buybreal) + 0.8*batmax
  if(sum((batnow>batmax) + (batnow<0)) >0 ){stop("Error1!")}
  return(list(time = as.duration(now()-time0), 
              realProfit = calc_profit3(real_iny,real_outy,batnow)))
}
calc_profit3= function(real_iny, real_outy, batnow){
  batnowreal = 0.8 * batmax
  Batreal = rep(NA, nx) 
  buyb = rep(NA,nx)
  for(i in (1:nx)){
    IO = real_iny[i] - real_outy[i]
    target = batnow[i] - batnowreal
    if(IO>=0){
      if(target>=0){
        batall =  min(target/batrateio,  (batmax - batnowreal)/batrateio, bat15max)
        Batreal[i] = min(batall, IO)
        buyb[i] = batall-Batreal[i]
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
        buyb[i] = min(target/batrateio, (batmax - batnowreal)/batrateio, bat15min)
      }
    }
    batall = Batreal[i] + buyb[i]
    batnowreal = batnowreal + ifelse(batall>0,batall*batrateio, batall)
  }
  Sellreal = real_iny - real_outy - Batreal
  if(sum(Batreal * Sellreal<0)>0){stop("Error2!")}
  batallall = Batreal+buyb
  return(
    sum(ifelse(Sellreal>0, sellp, buyp) * Sellreal) +
      -batdepre * sum(abs(batallall))+
      subsidy_all + 
      ifelse(sum(batallall)<0, sum(batallall)*mean(buyp), sum(batallall)*mean(sellp))
  )
}

#################
# calculation for "all bat" strategy:
calc_profit_all_bat = function(real_iny, real_outy){
  batnowreal = 0.8 * batmax
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
  return(
    sum(ifelse(Sellreal>0, sellp, buyp) * Sellreal) +
      -batdepre * sum(abs(Batreal))+
      subsidy_all +
      ifelse(sum(Batreal)<0, sum(Batreal)*mean(buyp), sum(Batreal)*mean(sellp))
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
# Solve for a PSO result
# 德国模式：完全不允许电池、电网交互

# 中间模式：允许电网向电池充电，不允许电池向电网卖电

# 国内模式：完全运行电网、电池交互


# public variables:
subsidy = 0.42 * (1-0.17) * (1-0.25) # 补贴，减去增值税和所得税
buy = function(hour){
  # 上海市电网夏季销售电价表（单一制分时电价用户）工商业及其他用电 http://www.sgcc.com.cn/dlfw/djzc/
  return(ifelse(hour>=6 & hour<22, 1.044, 0.513))
}
sell = 0.85-subsidy # minus subsidy, because it always exists
hours = sort(rep(0:23,4))
buyp = buy(hours)
sellp = rep(sell, 96)
batmax = 10000 # 电池容量(kwh)
punish = 1e10 # 过量惩罚系数
bat15min = -0.2 * batmax /4 # 过量惩罚系数
bat15max = 0.2 * batmax /4 # 每15分钟最大充电/放电值: 每小时充放电容量不能超过其最大容量的 20%
batdepre = 0.1# 电池折旧成本／（kW·h）
batrateio = 0.8 # 电池充放电效率
no_improvement = 2 # how much profit increase is condidered to be no improvement
control = list(maxit = 3000, trace=F, REPORT=500) # the control for psoptim()

#%%
# main program
df = read.csv('out.csv')
iny = df$iny
outy = df$outyp
real_iny = df$iny
real_outy = df$outytrue
nx=length(iny)
result1 = PSOsolve(iny,outy,real_iny,real_outy)
outy = real_outy # suppose we know the real data
result2 = PSOsolve(iny,outy,real_iny,real_outy)
output = data.frame(time = as.numeric(c(result1$time,result2$time,0,0),'s'), 
           profit= c(result1$realProfit,result2$realProfit,
                     calc_profit_all_bat(real_iny, real_outy),
                     calc_profit_no_bat(real_iny, real_outy)))
write.csv(output, 'Routput.csv')
