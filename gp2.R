rm(list=ls())
library(ploty)
library(quantmod)
library(tseries)
library(gramEvol)
library(lubridate)
library(knitr)
#install.packages("gtrendsR")
library(gtrendsR)
#install.packages('Quandl')
library(Quandl)
BTC=get(getSymbols('BTC-USD',from="2019-01-01", to="2020-01-01"))
bt=gtrends('bitcoin')
btrend=xts(bt$interest_over_time$hits,order.by = (bt$interest_over_time$date))

#xin=1/(1+exp(-x))
new=function(x){
  a=(1/(1+exp(-x)))
  return(a)}
old=function(x){
  #x=as.double(x)
  a=(-log(1/x-1))
  return(a)}
#x=-log(1-1/xin)


#head(bt$interest_over_time)

MA5=SMA(BTC[,3],n=5 )
MA10=SMA(BTC[,3],n=10 )
MA20=SMA(BTC[,3],n=20 )
diff0=diff(BTC$`BTC-USD.Close`,lag = 1)

data=merge(diff0,MA5,MA10,MA20,BTC$`BTC-USD.Close`,BTC$`BTC-USD.Volume`,btrend)
colnames(data)=c('dif','ma5','ma10','ma20','close','vol','ggl')
head(data)
l.m5=(1+Lag(MA5,1))
l.m10=(1+ Lag(MA10,1)) 
l.m20=(1+ Lag(MA20,1)) 

lg1=(1+Lag(BTC$`BTC-USD.Close`,1))  
lv1= (1+Lag(BTC$`BTC-USD.Volume`,1))
diff2=Lag(diff(BTC$`BTC-USD.Close`,lag = 2),1)
diff1=Lag(diff(BTC$`BTC-USD.Close`,lag = 1),1)
diff3=diff((BTC$`BTC-USD.Close`))
diff4=diff((BTC$`BTC-USD.Volume`))

td=merge( l.m5 , l.m10 , l.m20 , lg1 , lv1)#traindata
td=na.omit(td)
length(td)
#save=td
colnames(td)=c('ma5','ma10','ma20','close','vol')
#td=save
td=first(td,length(td$dif)-30)
td=(td/10)
forecastdata=first(data,length(td$dif))




##rules

newRules <- list(expr = grule(op(expr, expr), func(expr), var),
                 func = grule(sin, cos, exp, log,sqrt),
                 op = grule('+', '-', '*', '/', '^'),
                 n = grule(1,2,3,4,5),
                 var = grule( td$ma5,td$ma10 , td$ma20,td$vol)
)

# Then need to create grammar from rules
# Need to be clear about this step
newGram <- CreateGrammar(newRules)
newGram
comp=((BTC$`BTC-USD.Close`))
  #log((BTC$`BTC-USD.Close`/10))
#fitness function (RMSE)
newFitFunc <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result)))
    return(Inf)
  return (sqrt(mean(comp - result)^2))
}



#GrammaticalEvolution
ge <- GrammaticalEvolution(newGram, newFitFunc, terminationCost = 0.05, max.depth = 5)
ge
best=ge$best$expressions
dd=list()

best=ge$best$expressions

plot((eval (best)))
lines(comp,col='red')
plot(comp,col='red')
dtest=merge((eval(best)), comp)

dtest=na.omit(dtest)
plot(dtest)
plot(last(dtest,50))
iff=merge(Lag(dtest[,1],0),Lag(dtest[,2]),1)
plot(iff)
iff=na.omit(iff)
sqrt(mean((eval(best) - comp)^2))

pd=first(td$close,200)
l=length(pd)
for (i in 1:30) {
  k= last(eval(ge$best$expressions))
  pd=rbind.xts(pd,k)
  MA5=SMA(pd,n=5 )
  MA10=SMA(pd,n=10 )
  MA20=SMA(pd,n=20 )
  l.m5=MA5
  l.m10=MA10
  l.m20=MA20
  
  lg1=Lag(pd,1)
  lv1=Lag(pd,1)
  diff1=diff(pd,lag = 1)
  
  td=merge(diff1 , l.m5 , l.m10 , l.m20 , lg1 , lv1)#traindata
  td=na.omit(td)
  colnames(td)=c('dif','ma5','ma10','ma20','close','vol')
  if (l+i>=length(data$dif)) break
}  

dates <- seq(date(first(pd)), length = length(pd), by = "days")
pd=xts(coredata(pd),dates)
plot(pd)

fore=na.omit( merge(pd,BTC$`BTC-USD.Close`) )


fore
plot(fore)
f=last(fore,80)
sqrt(mean((f[,1] - f[,2])^2))
plot(f)
plot(last(fore,30))
k=na.omit(k)

library(ggplot2)
ggplot(m,aes(na,hit))+
geom_boxplot()
k=na.omit(bt$interest_by_country)  
m=data.frame(na=as.vector(k[,1]),hit=as.vector(k[,2]))
plot(m)
