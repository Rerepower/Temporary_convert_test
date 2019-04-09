library(quantmod)
library(TTR)
library(PerformanceAnalytics)
#rate of return of stocks
symbols <- c("GOOG","FB","MSFT","INTC","T","VZ","CSCO"
             ,"ORCL","NVDA","JPM","BAC","WFC","V","BRK-B",
             "C","MA","AMZN","WMT","HD","DIS","NFLX","AAPL","PG", 
             "KO","NKE","XOM","CVX","DWDP","JNJ","PFE","MRK","UNH", 
             "CVS","AMGN","BA","GE","MMM")
getSymbols(symbols, src='yahoo', from = '2015-09-01',to = '2018-08-31')
prices <- list()
for(i in 1:length(symbols)) {
  tmp <- Ad(get(symbols[[i]]))
  prices[[i]] <- tmp
}
prices <- do.call(cbind, prices)

#monthly return
monthlyP<-list()
for(i in 1:length(symbols)){
  tmp<-monthlyReturn(prices[,i])
  monthlyP[[i]]<-tmp
}
monthlyP<-do.call(cbind,monthlyP)

#daily return
returns<-list()
for(i in 1:length(symbols)) {
  tmp <- CalculateReturns(prices[,i], method="log")
  returns[[i]] <- tmp
}
returns <-do.call(cbind,returns)

#SMA20 
SMA20<-list()
for(i in 1:length(symbols)){
  tmp<-SMA(prices[,i],n=20)
  SMA20[[i]]<-tmp
  # tmpname<-names(returns)[i]
  # names(SMA20)[[i]]<-tmpname
}
SMA20<-do.call(cbind,SMA20)

#EMA14
EMA14<-list()
for(i in 1:length(symbols)){
  tmp<-EMA(prices[,i],n=14)
  EMA14[[i]]<-tmp
}
EMA14<-do.call(cbind,EMA14)

#RSI14
RSI14<-list()
for(i in 1:length(symbols)){
  tmp<-RSI(prices[,i],n=14)
  RSI14[[i]]<-tmp
}
RSI14<-do.call(cbind,RSI14)

#MACD 2 col per stock
MACD<-list()
for(i in 1:length(symbols)){
  tmp<-MACD(prices[,i],nFast=12, nSlow=26, nSig=9, maType=SMA)
  MACD[[i]]<-tmp
}
MACD<-do.call(cbind,MACD)

#SMA,ENA,RSI,MACD
GOOG.Adjusted<-returns$GOOG.Adjusted
GOOGSMA<-SMA20$SMA.1
GOOGEMA<-EMA14$EMA.1
GOOGRSI14<-RSI14$rsi.1
GOOGMACD<-MACD$macd.1
GOOGdata<-cbind(GOOG.Adjusted,GOOGSMA,GOOGEMA,GOOGRSI14,GOOGMACD)
GOOGdata<-na.omit(GOOGdata)
head(GOOGdata)
tail(GOOGdata)
GOOG.Adjusted<-GOOGdata$GOOG.Adjusted[-1]
GOOGSMA<-GOOGdata$SMA.1[-730]
GOOGEMA<-GOOGdata$EMA.1[-730]
GOOGRSI14<-GOOGdata$rsi.1[-730]
GOOGMACD<-GOOGdata$macd.1[-730]
GOOGREG<-lm(GOOG.Adjusted~GOOGSMA+GOOGRSI14+GOOGEMA+GOOGMACD)
summary(GOOGREG)

## Fama French 5 factor model daily
FF.raw <-read.fwf(file="C:/Users/kid00/iCloudDrive/2018 Fall/FIN 6392/F-F_Research_Data_5_Factors_2x3_daily.txt",width=c(8,8,8,8,8,8,8),skip=4)
FF.raw<-FF.raw[-1:-13132,] 
names(FF.raw) <-paste(c("text.date","RmxRf","SMB","HML","RMW","CMA","Rf"))
str(FF.raw)


FF.raw$RmxRf <-as.numeric(as.character(FF.raw$RmxRf))/100
FF.raw$Rf <-as.numeric(as.character(FF.raw$Rf))/100
FF.raw$SMB <-as.numeric(as.character(FF.raw$SMB))/100
FF.raw$HML <-as.numeric(as.character(FF.raw$HML))/100
FF.raw$RMW <-as.numeric(as.character(FF.raw$RMW))/100
FF.raw$CMA <-as.numeric(as.character(FF.raw$CMA))/100

## calculate excessive return

FF.raw$exret <-returns$GOOG.Adjusted-FF.raw$Rf
exret1<-FF.raw$exret[-1]
RmxRf<-FF.raw$RmxRf[-756]
SMB<-FF.raw$SMB[-756]
HML<-FF.raw$HML[-756]
RMW<-FF.raw$RMW[-756]
CMA<-FF.raw$CMA[-756]
FF_5factor <-lm(exret1 ~ RmxRf + SMB + HML+ RMW + CMA)
summary(FF_5factor)

## Fama French 5 factor model monthly
FF.rawm <-read.fwf(file="C:/Users/kid00/iCloudDrive/2018 Fall/FIN 6392/F-F_Research_Data_5_Factors_2x3.txt",width=c(6,8,8,8,8,8,8),skip=4)
FF.rawm<-FF.rawm[-1:-626,] 
names(FF.rawm) <-paste(c("text.date","RmxRf","SMB","HML","RMW","CMA","Rf"))
str(FF.rawm)


FF.rawm$RmxRf <-as.numeric(as.character(FF.rawm$RmxRf))/100
FF.rawm$Rf <-as.numeric(as.character(FF.rawm$Rf))/100
FF.rawm$SMB <-as.numeric(as.character(FF.rawm$SMB))/100
FF.rawm$HML <-as.numeric(as.character(FF.rawm$HML))/100
FF.rawm$RMW <-as.numeric(as.character(FF.rawm$RMW))/100
FF.rawm$CMA <-as.numeric(as.character(FF.rawm$CMA))/100

## calculate excessive return

FF.rawm$exret <-monthlyP$monthly.returns-FF.rawm$Rf
exret1m<-FF.rawm$exret[-1]
RmxRfm<-FF.rawm$RmxRf[-36]
SMBm<-FF.rawm$SMB[-36]
HMLm<-FF.rawm$HML[-36]
RMWm<-FF.rawm$RMW[-36]
CMAm<-FF.rawm$CMA[-36]
FF_5factorm <-lm(exret1m ~ RmxRfm + SMBm + HMLm+ RMWm + CMAm)
summary(FF_5factorm)

#alpha
## Fama French 5 factor model daily
FF.raw <-read.fwf(file="C:/Users/kid00/iCloudDrive/2018 Fall/FIN 6392/F-F_Research_Data_5_Factors_2x3_daily.txt",width=c(8,8,8,8,8,8,8),skip=4)
FF.raw<-FF.raw[-1:-13132,] 
names(FF.raw) <-paste(c("text.date","RmxRf","SMB","HML","RMW","CMA","Rf"))
str(FF.raw)


FF.raw$RmxRf <-as.numeric(as.character(FF.raw$RmxRf))/100
FF.raw$Rf <-as.numeric(as.character(FF.raw$Rf))/100
FF.raw$SMB <-as.numeric(as.character(FF.raw$SMB))/100
FF.raw$HML <-as.numeric(as.character(FF.raw$HML))/100
FF.raw$RMW <-as.numeric(as.character(FF.raw$RMW))/100
FF.raw$CMA <-as.numeric(as.character(FF.raw$CMA))/100

FF.raw$exret<-returns$GOOG.Adjusted-FF.raw$Rf
FF_5factor <-lm(FF.raw$exret ~ RmxRf + SMB + HML+ RMW + CMA)
summary(FF_5factor)