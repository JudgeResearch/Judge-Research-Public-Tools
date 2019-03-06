# 1-JudgeReportUpdateMaster.R

# Reset Workspace
rm(list=ls())
options("scipen"= 20, "digits"=10)
showWarnings = FALSE
setwd("C:/Users/Yunpeng/Documents/GitHub/Judge-Research-Public-Tools")
# source("0-JudgeReportFunctionLib.R")
source(file.path(getwd(),"dataRetrieval","loadDailyCryptoData.R"))
source(file.path(getwd(),"dataRetrieval","loadSp500Data.R"))
source(file.path(getwd(),"dataCleaning","createPriceMatrix.R"))
source(file.path(getwd(),"dataCleaning","createReturnMatrix.R"))
source(file.path(getwd(),"dataCleaning","createDccData.R"))
source(file.path(getwd(),"dataCleaning","createCoinSummary.R"))
source(file.path(getwd(),"dataAnalysis","estimateDccMatrix.R"))
source(file.path(getwd(),"dataVisualization","plotDccdata.R"))
source(file.path(getwd(),"dataVisualization","plot30Dcc.R"))
source(file.path(getwd(),"dataVisualization","plot30Price.R"))
source(file.path(getwd(),"dataVisualization","plotExRev"))
source(file.path(getwd(),"dataVisualization","plotPriceVolume.R"))
source(file.path(getwd(),"dataVisualization","plotCorrMatrix.R"))


# Import required library A-Z ascending
if(!require(dygraphs)){install.packages("dygraphs"); library(dygraphs)} # graph presentation
if(!require(forecast)){install.packages("forecast"); library(forecast)} # time series/regression
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)} # ploting
if(!require(highcharter)){install.packages("highcharter"); library(highcharter)} # highchart package
if(!require(PerformanceAnalytics)){install.packages("PerformanceAnalytics"); library(PerformanceAnalytics)} # compute financial series
if(!require(quantmod)){install.packages("quantmod"); library(quantmod)} # time series analysis and chart
if(!require(reshape2)){install.packages("reshape2"); library(reshape2)} # matrix manipulation
if(!require(rmgarch)){install.packages("rmgarch"); library(rmgarch)} # garch package
if(!require(rsconnect)){install.packages("rsconnect"); library(rsconnect)} # shinny app
if(!require(schoolmath)){install.packages("schoolmath"); library(schoolmath)} # math operation
if(!require(sqldf)){install.packages("sqldf"); library(sqldf)} # sql script
if(!require(tikzDevice)){install.packages("tikzDevice"); library(tikzDevice)} # graph presentation
if(!require(TSA)){install.packages("TSA"); library(TSA)} # time series analysis 
if(!require(xts)){install.packages("xts"); library(xts)}  # time series analysis
if(!require(zoo)){install.packages("zoo"); library(zoo)} # time series

# Folder and File Path Configuration

folderCmc<-file.path(getwd(), "data_cmc_19.02.07")
folderSp<-file.path(getwd(), "data_sp")
folderInput<-file.path(getwd(), "input")
folderDccPic<-file.path(getwd(), "dcc_chart_19.02.07")
folderPic<-file.path(getwd(), "charts")
# ========================================
tokenListFile<-"final 30 crypto v4.csv"
sp500DataFile<-"S&P 500 Historical Data v5.csv"
startdate='2016-01-01'  # 2016-01-01 
enddate='2019-02-06'
startIndex1<-731 # Jan 1 2018
startIndex2<-790 # Mar 1, 2018
endIndex<-1132 # last data point index
twindow <-0
# =======================================

# run scripts in sequence
source("1-JudgeDataPrepare.R")
source("2-JudgeModelPrepare.R")
source("3-JudgeChartPrepare.R")
source("4-JudgeTablePrepare.R")

# save charts in the folder for Judge Report
file.copy(file.path(folderDccPic, "BTCusdETHusdDCC.eps"), folderPic, overwrite = TRUE)
file.copy(file.path(folderDccPic, "BTCusdNEOusdDCC.eps"), folderPic, overwrite = TRUE)
file.copy(file.path(folderDccPic, "BNBusdBTCusdDCC.eps"), folderPic, overwrite = TRUE)
file.copy(file.path(folderDccPic, "ICXusdBTCusdDCC.eps"), folderPic, overwrite = TRUE)
file.copy(file.path(folderDccPic, "XLMusdBNBusdDCC.eps"), folderPic, overwrite = TRUE)
file.copy(file.path(folderDccPic, "DASHusdXRPusdDCC.eps"), folderPic, overwrite = TRUE)
# file.copy(file.path(folderDccPic, "BTCusdsp500usdDCC.eps"), folderPic, overwrite = TRUE)

# copy charts folder to judge report folder
file.copy(folderPic, file.path(pathFolder,"judge_report"), recursive=TRUE, overwrite = TRUE)

# copy table data sets to judge report folder
file.copy(file.path(pathFolder,"table_r.rdata"), file.path(pathFolder,"judge_report"), overwrite = TRUE)
file.copy(file.path(pathFolder,"ytdvar.rdata"), file.path(pathFolder,"judge_report"), overwrite = TRUE)

