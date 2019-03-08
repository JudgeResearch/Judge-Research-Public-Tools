# 1-JudgeReportUpdateMaster.R

# Reset Workspace
rm(list=ls())
options("scipen"= 20, "digits"=10)
showWarnings = FALSE
setwd("~/Personal/CryptoAnalytics/Telegram Text Mining/")
getwd()
source("teleTextFuncLib.R")

# load library
if(!require(dygraphs)){install.packages("dygraphs"); library(dygraphs)} # graph presentation
if(!require(xts)){install.packages("xts"); library(xts)}  # time series analysis
if(!require(sqldf)){install.packages("sqldf"); library(sqldf)} # sql script
if(!require(zoo)){install.packages("zoo"); library(zoo)} # time series
if(!require(rsconnect)){install.packages("rsconnect"); library(rsconnect)} # shinny app
if(!require(highcharter)){install.packages("highcharter"); library(highcharter)} # highchart package
if(!require(quantmod)){install.packages("quantmod"); library(quantmod)} # time series analysis and chart
if(!require(reshape2)){install.packages("reshape2"); library(reshape2)} # matrix manipulation
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)} # ploting
if(!require(forecast)){install.packages("forecast"); library(forecast)} # time series/regression
if(!require(PerformanceAnalytics)){install.packages("PerformanceAnalytics"); library(PerformanceAnalytics)} # compute financial series
if(!require(TSA)){install.packages("TSA"); library(TSA)} # time series analysis 
if(!require(rmgarch)){install.packages("rmgarch"); library(rmgarch)} # garch package
if(!require(schoolmath)){install.packages("schoolmath"); library(schoolmath)} # math operation
if(!require(plyr)){install.packages("plyr"); library(plyr)} # math operation
if(!require(anytime)){install.packages("anytime"); library(anytime)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(sentimentr)){install.packages("sentimentr"); library(sentimentr)} # sentiment analysis
if(!require(tm)){install.packages("tm"); library(tm)} # sentiment analysis
# if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)} # math operation
if(!require(psych)){install.packages("psych"); library(psych)}
# if(!require(car)){install.packages("car"); library(car)}
if(!require(randomForest)){install.packages("randomForest"); library(randomForest)} # randomForest pacakge
if(!require(TTR)){install.packages("TTR"); library(TTR)} # technical indicator

# =======================================================
# define trading strategy
startdate="2018-02-01"
enddate="2019-01-11"
dataNum=6 # data points in a day
# trxFee=5
balance0=10000
# buyprice<-1
# rsiBuy<-40
# rsiSell<-70
# minR<-1.2

# =======================================================
# load data
load("coinlist2.rdata")
coinlist2[6,2]<-"XRP"
coinlist2[7,2]<-"BNB"
coinlist2[8,2]<-"ETH"
coinlist2[6,3]<-"USD"
coinlist2[7,3]<-"USDT"
coinlist2[8,3]<-"USD"
# coinlist2<-coinlist[c(3,6,9,18,28),]
# save(coinlist2, file="coinlist2.rdata")
SMA_l<-c(60, 90, 120)
SMA_s<-c(6, 12, 24)

j<-7
m<-2
n<-3
for (j in (6:length(coinlist2$ticker))) {
  load(paste0("./rdata/", coinlist2$ticker[j],"_", coinlist2$base[j],"_4hour.rdata"))
  # jointData$date<-as.Date(jointData$date, format="%m/%d/%Y")
  # jointData$datetime<-as.POSIXct(jointData$datetime, format="%m/%d/%Y %H:%M")
  jointData$date<-anydate(jointData$date, asUTC =TRUE)
  jointData$datetime<-as.POSIXct(jointData$datetime,origin="1970-01-01",tz="GMT")
  # =======================================================
  # calculate technical indicators - parameter optimization
  
  trading_table<-as.data.frame(matrix(ncol=5, nrow=length(SMA_l)*length(SMA_s)))
  colnames(trading_table)<-c("ticker", "base", "SMA_short", "SMA_long", "Total_Return")
  trading_table$ticker<-coinlist2$ticker[j]
  trading_table$base<-coinlist2$base[j]
  for (m in (1:length(SMA_l))){
    for (n in (1:length(SMA_s))){
      # jointData$rsiMA1<-RSI(jointData$close, maType="WMA", wts=jointData$volumefrom)
      jointData$SMA_short<-SMA(jointData$close, n=SMA_s[n], maType="WMA", wts=jointData$volumefrom)
      jointData$SMA_long<-SMA(jointData$close, n=SMA_l[m], maType="WMA", wts=jointData$volumefrom)
      
      # select data
      startrow<-match(as.Date(startdate), jointData$date)
      endrow<-match(as.Date(enddate), jointData$date)+dataNum-1
      
      jointData2<-jointData[startrow:endrow,]
      row.names(jointData2) <- 1:nrow(jointData2)
      
      # execute trading strategy
      # buy when SMA10 crosses SMA100
      # sell when SMA10 crosses SMA100
      tradeAccount<-tradeExec(startrow, endrow, balance0, jointData2$SMA_short, jointData2$SMA_long, jointData2)
      save(tradeAccount, file=file.path(getwd(), "rdata", paste0(coinlist2$ticker[j],"_", coinlist2$base[j],"_4h_trading.rdata")))
      #plot the graph
      
      postscript(file.path("./chart",paste0(coinlist2$ticker[j],"_",coinlist2$base[j], 
                           "_trading_SMA_", SMA_l[m], "_", SMA_s[n], ".eps")), fonts="serif",
                 width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")
      
      tradeGraph(jointData2, tradeAccount, balance0)
      
      dev.off()
      
      # update trading_table
      trading_table[(m-1)*length(SMA_s)+n,3]<-SMA_s[n]
      trading_table[(m-1)*length(SMA_s)+n,4]<-SMA_l[m]
      trading_table[(m-1)*length(SMA_s)+n,5]<-tradeAccount$total[length(tradeAccount$total)]
      message(m,n)
    }
  }
  trading_table$max<-""
  trading_table$max[match(max(trading_table$Total_Return),trading_table$Total_Return)]<-"*"
  print(trading_table)
  message(coinlist2$ticker[j], " optimization is done correctly~!")
}

