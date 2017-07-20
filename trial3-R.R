# These are the functions for "trial5.R"
library(lubridate)
library(pso)

## PSO -----------------

# 适应度函数（越小越好） - output is the cost+punish
evaluate = function(Batrate){
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
# given Bat, iny, outy, calculate the profit:
calc_profit= function(real_iny, real_outy, batnow){
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
  #plot the bat
  #plot(batnow,type='l',ylim=c(0,batmax))
  #lines(cumsum(ifelse(Batreal>0,Batreal*batrateio,Batreal))+0.8*batmax,col='red')
  
  return(
    sum(ifelse(Sellreal>0, sellp, buyp) * Sellreal) +
      -batdepre * sum(abs(Batreal))+
      subsidy_all + 
      ifelse(sum(Batreal)<0, sum(Batreal)*mean(buyp), sum(Batreal)*mean(sellp))
  )
}
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
PSOsolve = function(iny,outy, real_iny, real_outy){
  PSOresult = list(par=rep(1,nx))
  subsidy_all <<- sum(real_iny) * subsidy
  time0 = now()
  cost=c(evaluate(rep(0.5,nx)))
  i = 1 # the n.row of cost
  while(i<2 || cost[i-1]-cost[i]>no_improvement){
    PSOresult = psoptim(PSOresult$par,evaluate,lower = rep(0,nx),upper=rep(1,nx),
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
              Bat = Bat,
              Batreal = Batreal,
              realProfit = calc_profit(real_iny,real_outy,batnow)))
}

MCforOpt = function(df = MCdf, tb = MCresult, method = "prediction"){
  ntimes = nrow(MCresult)
  for(i in 1:ntimes){
    real_iny <<- df$iny[df$date==tb$indate[i]]
    real_outy <<- df$outy[df$date==tb$outdate[i]]
    iny <<- real_iny
    if(method=="real"){outy <<- real_outy}else{outy <<- LMpredict(df, tb$outdate[i])}
    #plot the prediction
    plot(real_outy,type='l', main='Prediction Effectiveness')
    lines(outy, col='red')
    nx <<- length(iny)
    #print(ggplot(NULL,aes(x=1:96,y=iny))+geom_line(color=2)+geom_line(aes(x=1:96,y=outy),color=3))
    PSOresult= PSOsolve(iny, outy, real_iny, real_outy)
    tb$profit[i] = PSOresult$realProfit
    tb$time[i] = PSOresult$time
    tb$solution[i] = list(PSOresult$Bat)
    #print(ggplot(NULL,aes(x=1:96,y=cumsum(tb$solution[[i]])))+geom_line(color=2))
    # all bat:
    tb$all_bat[i] = calc_profit_all_bat(real_iny, real_outy)
    # no bat:
    tb$no_bat[i] = calc_profit_no_bat(real_iny, real_outy)
    message("#####################################")
    message(i/ntimes)
    message("#####################################")
  }
  rm(real_iny,real_outy,iny,outy,subsidy_all,nx,envir = .GlobalEnv)
  return(tb)
}

#MCtimes = 20 # how many times the MC will run?
#daylist = unique(MCdf$date) # the list that you can select from
#daylist = daylist[50:length(daylist)] # atlist 50 days for training
#MCresult = tibble(n=1:MCtimes, 
                  # indate = sample(daylist,MCtimes,replace = T),
                  # outdate = sample(daylist,MCtimes,replace = T),
                  # solution=NA, profit=NA, all_bat=NA, no_bat=NA, time=NA)
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
outy = real_outy
result2 = PSOsolve(iny,outy,real_iny,real_outy)
output = data.frame(time = as.numeric(c(result1$time,result2$time,0,0),'s'), 
           profit= c(result1$realProfit,result2$realProfit,
                     calc_profit_all_bat(real_iny, real_outy),
                     calc_profit_no_bat(real_iny, real_outy)))
write.csv(output, 'Routput.csv')