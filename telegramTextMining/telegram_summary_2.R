# telegram_summary.R

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
if(!require(crypto)){install.packages("crypto"); library(crypto)} # crypto package

# =======================================================
# load data
load("coinlist.rdata")
# coinlist<-coinlist[1:28,]
startdate="20160101"
enddate="20190314"
startIndex<-4965 #790 # Mar 1, 2018
endIndex<-6686 # last data point index

# create a summary with market cap info
coinTeleSummary <- as.data.frame(matrix(ncol=2 , nrow=length(coinlist$ticker)))
colnames(coinTeleSummary)=c("coin", "market_cap")
coinTeleSummary$coin<-coinlist$ticker
i<-1
for (i in (1:length(coinlist$ticker))){
  data_temp<-crypto_history(coin = coinlist$ticker[i], start_date = startdate, end_date = enddate)
  coinTeleSummary$market_cap[i]<-data_temp$market[length(data_temp$market)]
  message(i)
}
save(coinTeleSummary, file="./rdata/coinTeleSummary.rdata")

# =====================================
# create 29 message count matrix
load("./rdata/BTC_USD_4hour.rdata")
jointData$date<-anydate(jointData$date, asUTC =TRUE)
jointData$datetime<-as.POSIXct(jointData$datetime,origin="1970-01-01",tz="GMT")
data_tele<-as.data.frame(jointData$datetime)
colnames(data_tele)<-"datetime"
data_tele$BTC_USD<-jointData$close
i<-1
for (i in (1:length(coinlist$ticker))){
  dfname<-paste0(coinlist$ticker[i],"_", coinlist$base[i])
  load(paste0("./rdata/", dfname, "_4hour.rdata"))
  datatemp<-createTeleMatrix("jointData")
  data_tele <- sqldf(paste0("select a.*, b.datatemp as ", 
                         paste0(dfname, "_tele"), " from data_tele a left join datatemp", 
                         " b on a.datetime=b.datetemp"))
  message(i)
}
save(data_tele, file="data_tele.rdata")
message("telegrame matrix is complete")
# ====================================
# plot 29 lines of message count in one chart

postscript(file.path("./chart",paste0("28TeleMessageCountMar2018.eps")), fonts="serif",
           width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")
par(
  lty = 1, 		  		  # lty = "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"			  
  lwd = 1,				  # function "lines" takes a vector of values for more than one line, and will cycle through that vector
  pch = ".",				  # point types
  bty = "n",								#suppresses box around the plot, keep part by letters like "c" 
  cex.axis = 1.5, col.axis = grey(.6) 	, # font.axis = ,		#size, color and font of axis text
  xaxt ="s", 
  yaxt ="s",	 		  # s=standard, n=suppress axis
  
  cex.lab  = 1.5, col.lab = grey(.6),  font.lab  = 6,	 #axis label size, color and font
  cex.main =  1.2, col.main= grey(.4),  font.main = 8,		#main label, color and font
  cex.sub  =  1.3, col.sub = grey(.4),  font.sub  = 6,		#sub label, color and font
  
  #tick marks
  lab = c(x=10,y=10,len=.1)	, # las = 	,		#number of ticks, las = axis labels 0 = default, 1=horizontal, 2 = perpendicular, 3 = vertical
  tck = -.01  			, # negative is external ticks, 1 is gridlines
  lend = "square"			, # tick mark end "round" default, "butt" butt lines, "square" square line caps
  fg = grey(.2),	      	  # foreground color
  bg = grey(.95),
  family= "serif",
  # https://nicercode.github.io/guides/plotting/
  # bottom, left, top, right
  mar=c(10,6,2,4)
)

i=2
max_vec=rep(0,length(coinlist$ticker))
for (i in 1:length(coinlist$ticker)){
  data1_tele<-data_tele[startIndex:endIndex,i+2]
  # price_vec<-data1_tele*(1/data1_tele[max(which(data1_tele>0)[1],1)])
  max_vec[i]<-max(data1_tele, na.rm = TRUE)
}

max_max_vec <- max(max_vec)
color_vec=character()


data1_tele<-data_tele[startIndex:endIndex,2]
price_vec<-data1_tele
date_vec<-data_tele$datetime[startIndex:endIndex]
time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b")
time_label<-time_label_y
timeFreq<-"month"

plot(as.Date(date_vec), price_vec, type="l",
     ylab="",
     xlab="",
     xaxt="n",
     yaxt="n",
     ylim=c(0, max(price_vec)),
     col.ticks="white", col="red", lwd=2)
axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = "month"),
          labels= time_label,col="white",tck=1)   
# strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b-%y")
axis(2,col="white",tck=1,col.ticks="white", at = seq(0, max(price_vec), by = 1000*round(max(price_vec)/5000)))
title(ylab="BTC Price", line=3, cex.lab=1.5)


color_vec[1]<-"gray50"

for (i in (1:length(coinlist$ticker))){

    data1_tele<-data_tele[startIndex:endIndex,i+2]
    price_vec<-data1_tele
    
    par(new=T)
    plot(as.Date(date_vec), price_vec, ylim=c(0, max_max_vec), type="l",col=colors()[i+10],
         xaxt="n",yaxt="n",xlab="",ylab="", lwd=0.25, ann=FALSE)
    # https://stackoverflow.com/questions/11775692/how-to-specify-the-actual-x-axis-values-to-plot-as-x-axis-ticks-in-r
    
    color_vec[i]<-colors()[i+10]

}
axis(4, col=grey(.95), at = seq(0, max_max_vec, by = 1000*ceiling(max_max_vec/5000)))
par(xpd=TRUE)
coord <- par("usr")

# add legend to the chart
legend(coord[1]+20, coord[3]-1800, col=color_vec,lty=1,
       legend=colnames(data_tele)[2:30],  
       text.col="gray50", cex=.75, pt.cex = .75, box.col = "gray95", box.lwd = 0,
       ncol =8, inset = c(0, 0), bty = "n")

dev.off()

# =======================================
# daily average
data_tele$date<-as.data.frame(jointData$date)
colnames(data_tele[,length(colnames(data_tele))])<-"date"
data_tele_d<-aggregate(data_tele[,2:30], list(data_tele$date),mean)
startIndex2<-829
endIndex2<-1145
data_tele_d$avg<-rowMeans(data_tele_d[,3:30], na.rm = TRUE)
data_tele_d$min<-apply(data_tele_d[,3:30], 1, FUN=min, na.rm=TRUE)
data_tele_d$max<-apply(data_tele_d[,3:30], 1, FUN=max, na.rm=TRUE)
data_tele_d$sd<-apply(data_tele_d[,3:30], 1, FUN=sd, na.rm=TRUE)
# data_tele_d$sd<-sd(as.numeric(unlist(data_tele_d[,3:30])), na.rm = TRUE)
save(data_tele_d,file="data_tele_d.rdata")

# plot the daily average telegram with band
postscript(file.path("./chart",paste0("Average28TeleMessageCountMar2018.eps")), fonts="serif",
           width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")
par(
  lty = 1, 		  		  # lty = "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"			  
  lwd = 1,				  # function "lines" takes a vector of values for more than one line, and will cycle through that vector
  pch = ".",				  # point types
  bty = "n",								#suppresses box around the plot, keep part by letters like "c" 
  cex.axis = 1.5, col.axis = grey(.6) 	, # font.axis = ,		#size, color and font of axis text
  xaxt ="s", 
  yaxt ="s",	 		  # s=standard, n=suppress axis
  
  cex.lab  = 1.5, col.lab = grey(.6),  font.lab  = 6,	 #axis label size, color and font
  cex.main =  1.2, col.main= grey(.4),  font.main = 8,		#main label, color and font
  cex.sub  =  1.3, col.sub = grey(.4),  font.sub  = 6,		#sub label, color and font
  
  #tick marks
  lab = c(x=10,y=10,len=.1)	, # las = 	,		#number of ticks, las = axis labels 0 = default, 1=horizontal, 2 = perpendicular, 3 = vertical
  tck = -.01  			, # negative is external ticks, 1 is gridlines
  lend = "square"			, # tick mark end "round" default, "butt" butt lines, "square" square line caps
  fg = grey(.2),	      	  # foreground color
  bg = grey(.95),
  family= "serif",
  # https://nicercode.github.io/guides/plotting/
  # bottom, left, top, right
  mar=c(8,6,2,4)
)


data1_tele<-data_tele_d[startIndex2:endIndex2,2]
price_vec<-data1_tele
date_vec<-as.Date(data_tele_d$Group.1[startIndex2:endIndex2])
time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b")
time_label<-time_label_y
timeFreq<-"month"

data_tele_d$upper<-data_tele_d$avg+data_tele_d$sd*2
max_v<-max(data_tele_d$upper[startIndex2:endIndex2])
plot(as.Date(date_vec), data_tele_d$avg[startIndex2:endIndex2], 
     xaxt="n",yaxt="n",xlab="",ylab="", ylim=c(0,max_v), type = "l", col="blue")
polygon(c(as.Date(date_vec),rev(as.Date(date_vec))),
        c(data_tele_d$min[startIndex2:endIndex2],
          rev(data_tele_d$upper[startIndex2:endIndex2])),col = "grey75", border = TRUE,
        xaxt="n",yaxt="n",xlab="",ylab="", ylim=c(0,max_v))
lines(as.Date(date_vec), data_tele_d$avg[startIndex2:endIndex2], 
      xaxt="n",yaxt="n",xlab="",ylab="", 
      lwd = 1, col = "blue", ylim=c(0,max_v))
axis(4, col=grey(.95), at = seq(0, max_v, by = 100*round(2*max_v/500)))

par(new=T)
plot(as.Date(date_vec), price_vec, type="l",
     ylab="",
     xlab="",
     xaxt="n",
     yaxt="n",
     ylim=c(0, max(price_vec)),
     col.ticks="white", col="red", lwd=2)
axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = "month"),
          labels= time_label,col="white",tck=1)   
# strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b-%y")
axis(2,col="white",tck=1,col.ticks="white", at = seq(0, max(price_vec), by = 1000*round(max(price_vec)/5000)))
title(ylab="BTC Price", line=3, cex.lab=1.5)

par(xpd=TRUE)
coord <- par("usr")

legend(coord[1]+(coord[2]-coord[1])/5, coord[3]-coord[4]/5, col=c("red","blue", "grey75"),lty=1,
       legend=c("BTC_USD","Average Msg# 28 Coins", "Avg+SD Msg# 28 Coins"),  
       text.col="gray50", cex=1, pt.cex = 1, box.col = "gray95", box.lwd = 0,
       ncol =3, inset = c(0, 0), bty = "n")

dev.off()























