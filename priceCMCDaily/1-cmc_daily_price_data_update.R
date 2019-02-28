# 1. cmc data update

# Reset Workspace
rm(list=ls())
options("scipen"= 20, "digits"=10)
showWarnings = FALSE


# input =================================================================================
startdate="20160101"
enddate="20190217"
startIndex1<-731 # Jan 1 2018
startIndex2<-1004 #790 # Mar 1, 2018
endIndex<-1143 # last data point index
# setwd("~/Personal/CryptoAnalytics/Crypto DCC Chart Auto v1")
setwd("C://Users//Yunpeng//Documents//GitHub//Judge-Research-Public-Tools")
cmc_coin<-read.csv(file.path(getwd(),"input","cryptos_2.csv"), header = TRUE, sep=",")
getwd()
twindow=0
source("0-JudgeReportFunctionLib.R")
source(file.path(getwd(),"dataRetrieval","loadDailyCryptoData.R"))
source(file.path(getwd(),"dataCleaning","createPriceMatrix.R"))
source(file.path(getwd(),"dataCleaning","createReturnMatrix.R"))
source(file.path(getwd(),"dataCleaning","createDccData_2.R"))
source(file.path(getwd(),"dataAnalysis","estimateDccMatrix_2.R"))
source(file.path(getwd(),"dataVisualization","plotDccdata.R"))
# =======================================================================================

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
# https://cran.r-project.org/web/packages/crypto/crypto.pdf
if(!require(crypto)){install.packages("crypto"); library(crypto)} # crypto package


# =======================================================
# scrape data from cmc
i<-1
for (i in (1:length(cmc_coin$coin))){
  data_temp<-crypto_history(coin = cmc_coin$coin[i], start_date = startdate, end_date = enddate)
  assign(paste0(cmc_coin$coin[i], "_USD"),data_temp)
  save(data_temp, 
       file=file.path(getwd(),"rdata",paste0("price_daily_cmc_", cmc_coin$coin[i],"_usd_",startdate, "-",enddate,".rdata")))
  datafile=file.path(getwd(),"data", paste0("price_daily_cmc_", cmc_coin$coin[i],"_usd_",startdate, "-",enddate,".csv"))
  write.table(
    data_temp,
    file = datafile,
    row.names = FALSE, na = "", col.names = TRUE, sep = ","
  )
  message(i)
}

# load("./rdata/BTC_USD.rdata")
message("cmc coin data is complete.")

