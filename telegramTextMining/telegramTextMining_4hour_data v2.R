# 1-JudgeReportUpdateMaster.R

# Reset Workspace
rm(list=ls())
options("scipen"= 20, "digits"=10)
showWarnings = FALSE
setwd("~/Personal/CryptoAnalytics/Telegram Text Mining/")
getwd()

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
# =======================================================

load("coinlist.rdata")
endIndex<-6866 # 2019.01.11
i<-1; j<-9
for (j in (12:length(coinlist$ticker))) {
  coin<-paste0(coinlist$ticker[j],"_", coinlist$base[j])
  priceFileName<-paste0("CCCAGG_", coin, "_4hour1_2019-02-05.csv")
  teleFileName<-coinlist$chatFile[j]

  # =======================================================
  # load message data file
  teleData<-read.csv(file.path(getwd(),"data", teleFileName), header=TRUE)
  dateCol<-grep("\\bdate\\b", colnames(teleData))
  userCol<-grep("\\bfrom.username\\b", colnames(teleData))
  textCol<-grep("\\btext\\b", colnames(teleData))
  teleDataShort<-teleData[,c(dateCol, userCol, textCol)] # column # of text can be different 
  
  # generate 4 hour timewindow
  teleDataShort$date2<-anydate(teleDataShort$date, asUTC =TRUE)
  teleDataShort$datetime<-as.POSIXct(teleDataShort$date,origin="1970-01-01",tz="GMT")
  teleDataShort$hour<-hour(teleDataShort$datetime)
  teleDataShort$hour_start<-floor(teleDataShort$hour/4)*4
  teleDataShort$hour_end<-(floor(teleDataShort$hour/4)+1)*4
  
  # count # message, # unique ID, # positive, # negative
  teleDataShort$date_4hour<-paste0(as.character(as.numeric(teleDataShort$date2)+25569), " ", as.character(teleDataShort$hour_start))
  teleDataMessage<-count(teleDataShort, "date_4hour")
  
  # teleDataShort$unique.id<-teleDataShort$from.username
  # teleDataShort$unique_date_4hour<-teleDataShort$date_4hour
  # hourIndex<-teleDataShort$date_4hour[!duplicated(teleDataShort$date_4hour)]
  # 
  # i<-1
  # for (i in (1:length(hourIndex))){
  #   uniqueTemp<-teleDataShort[which(teleDataShort$date_4hour==hourIndex[i]),c(9:10)]
  #   uniqueTemp[duplicated(uniqueTemp$unique.id),]<-""
  #   
  #   teleDataShort$unique.id[which(teleDataShort$date_4hour==hourIndex[i])]<-uniqueTemp$unique.id
  #   teleDataShort$unique_date_4hour[which(teleDataShort$date_4hour==hourIndex[i])]<-uniqueTemp$date_4hour
  # }
  # 
  # teleDataUniqueID<-count(teleDataShort,"unique_date_4hour")
  # teleDataUniqueID<-teleDataUniqueID[-1,]
  
  # score each text message  =====================need more work. 
  scoretemp<-sentiment_by(get_sentences(as.character(teleDataShort$text)))
  teleDataShort$score<-scoretemp$ave_sentiment
  teleDataShort$positiveDate<-""
  teleDataShort$positiveDate[which(teleDataShort$score>0)]<-teleDataShort$date_4hour[which(teleDataShort$score>0)]
  teleDataShort$negativeDate<-""
  teleDataShort$negativeDate[which(teleDataShort$score<0)]<-teleDataShort$date_4hour[which(teleDataShort$score<0)]
  # generate count of positive and negative
  teleDataPositiveMessage<-count(teleDataShort, "positiveDate")
  teleDataNegativeMessage<-count(teleDataShort, "negativeDate")

  
  # load price
  priceData<-read.csv(file.path(getwd(), "data", priceFileName), header=TRUE)
  priceData$hour<-hour(priceData$datetime)
  priceData$date_num<-as.numeric(as.Date(priceData$date))+25569
  priceData$date_4hour<-paste0(as.character(priceData$date_num)," ", as.character(priceData$hour))
  
  # create a joint data set with telegram text features
  jointData <- sqldf(paste0("select a.*, b.freq as teleFreq from priceData a left join teleDataMessage", 
                            " b on a.date_4hour=b.date_4hour"))
  
  # jointData <- sqldf(paste0("select a.*, b.freq as teleIDFreq from jointData a left join teleDataUniqueID", 
  #                           " b on a.date_4hour=b.unique_date_4hour"))
  
  jointData <- sqldf(paste0("select a.*, b.freq as positiveFreq from jointData a left join teleDataPositiveMessage", 
                            " b on a.date_4hour=b.positiveDate"))
  
  jointData <- sqldf(paste0("select a.*, b.freq as negativeFreq from jointData a left join teleDataNegativeMessage", 
                            " b on a.date_4hour=b.negativeDate"))
  
  jointData$teleFreq[which(is.na(jointData$teleFreq))]<-0
  # jointData$teleIDFreq[which(is.na(jointData$teleIDFreq))]<-0
  jointData$positiveFreq[which(is.na(jointData$positiveFreq))]<-0
  jointData$negativeFreq[which(is.na(jointData$negativeFreq))]<-0
  jointData<-jointData[1:endIndex,]
  save(jointData, file=file.path(getwd(), "rdata",paste0(coin, "_4hour.rdata")))
  write.table(
    jointData,
    file = file.path(getwd(), "rdata_csv", paste0(coin, "_4hour.csv")),
    row.names = FALSE, na = "", col.names = TRUE, sep = ","
  )
  message(j, "-", coin, ": 100% Complete.")
}
