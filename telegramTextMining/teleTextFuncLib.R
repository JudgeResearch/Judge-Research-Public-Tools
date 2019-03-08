# teleTextFuncLib.R

numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.1f", val)) }

per_format <- function(m){sprintf("%1.0f%%", 100*m)}

createTeleMatrix<-function(dfname){
  datetemp<- as.POSIXct(eval(parse(text = paste0(dfname, "$datetime"))),origin="1970-01-01",tz="GMT")
  datetemp<-datetemp
  datatemp <- eval(parse(text = paste0(dfname, "$teleFreq")))
  datatemp[which(is.infinite(datatemp))] <- NA
  datatemp<-cbind(datetemp, datatemp)
  datatemp<-as.data.frame(datatemp)
  return(datatemp)
}

tradeExec <- function(startrow, endrow, balance0, SMA_short, SMA_long, jointData2){
  
  # SMA_short<-SMA_short
  # SMA_long<-SMA_long
  tradeAccount<- as.data.frame(matrix(ncol=9, nrow=endrow-startrow+1))
  colnames(tradeAccount)<-c("date","datetime","balanceBegin","buy", "sell", "holding","balanceEnd", "total", "totalR")
  tradeAccount[,1]<-jointData$date[startrow:endrow]
  tradeAccount[,2]<-jointData$datetime[startrow:endrow]
  tradeAccount[,3:9]<-0
  tradeAccount$balanceBegin[1]<-balance0
  for (i in (1:length(tradeAccount$datetime))){
    # for (i in (1:700)){ 
    # && jointData2$teleFreq[i]>buysignal 
    if (i==1){
      tradeAccount$balanceBegin[i]<-balance0
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-0
      tradeAccount$balanceEnd[i]<-balance0
      tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    } else if(i==length(tradeAccount$datetime)){
      tradeAccount$balanceEnd[i]<-tradeAccount$balanceBegin[i]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$holding[i-1]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    } else if (tradeAccount$balanceBegin[i]>0 
               && SMA_short[i-1]<=SMA_long[i-1] && SMA_short[i]>=SMA_long[i]){
      tradeAccount$buy[i]<-1
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$balanceBegin[i]/jointData2$close[i]
      tradeAccount$balanceEnd[i]<-0
      tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
      buyprice<-jointData2$close[i]
      
    } else if (tradeAccount$holding[i-1]>0 && SMA_short[i-1]>=SMA_long[i-1] && SMA_short[i]<=SMA_long[i]){
      # && jointData2$rsiMA1[i]>rsiSell) && jointData2$close[i]>buyprice*minR  {
      tradeAccount$balanceEnd[i]<-jointData2$close[i]*tradeAccount$holding[i-1]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-1
      tradeAccount$holding[i]<-0
      tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
      
    } else {
      tradeAccount$balanceEnd[i]<-tradeAccount$balanceBegin[i]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$holding[i-1]
      tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    }
  }
  return(tradeAccount)
}

# add Tele to buying signal
tradeExecTele <- function(startrow, endrow, balance0, SMA_short, SMA_long, jointData2){
  
  # SMA_short<-SMA_short
  # SMA_long<-SMA_long
  tele_short<-jointData2$teleSMA50
  tele_long <-jointData2$teleSMA300
  
  tradeAccount<- as.data.frame(matrix(ncol=9, nrow=endrow-startrow+1))
  colnames(tradeAccount)<-c("date","datetime","balanceBegin","buy", "sell", "holding","balanceEnd", "total", "totalR")
  tradeAccount[,1]<-jointData$date[startrow:endrow]
  tradeAccount[,2]<-jointData$datetime[startrow:endrow]
  tradeAccount[,3:9]<-0
  tradeAccount$balanceBegin[1]<-balance0
  for (i in (1:length(tradeAccount$datetime))){
    # for (i in (1:700)){ 
    # && jointData2$teleFreq[i]>buysignal 
    if (i==1){
      tradeAccount$balanceBegin[i]<-balance0
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-0
      tradeAccount$balanceEnd[i]<-balance0
      tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    } else if(i==length(tradeAccount$datetime)){
      tradeAccount$balanceEnd[i]<-tradeAccount$balanceBegin[i]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$holding[i-1]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    } else if (tradeAccount$balanceBegin[i]>0 
               # && SMA_short[i-1]<=SMA_long[i-1] 
               && SMA_short[i]>=SMA_long[i]
               && tele_short[i]>tele_long[i]){
      tradeAccount$buy[i]<-1
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$balanceBegin[i]/jointData2$close[i]
      tradeAccount$balanceEnd[i]<-0
      tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
      buyprice<-jointData2$close[i]
      
    } else if (tradeAccount$holding[i-1]>0 && SMA_short[i-1]>=SMA_long[i-1] && SMA_short[i]<=SMA_long[i]){
      # && jointData2$rsiMA1[i]>rsiSell) && jointData2$close[i]>buyprice*minR  {
      tradeAccount$balanceEnd[i]<-jointData2$close[i]*tradeAccount$holding[i-1]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-1
      tradeAccount$holding[i]<-0
      tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
      
    } else {
      tradeAccount$balanceEnd[i]<-tradeAccount$balanceBegin[i]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$holding[i-1]
      tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    }
  }
  return(tradeAccount)
}

# shorting
tradeExecShort<-function(startrow, endrow, balance0, SMA_short, SMA_long, jointData2){
  # SMA_short<-jointData2$SMA_short
  # SMA_long<-jointData2$SMA_long
  tradeAccount<- as.data.frame(matrix(ncol=9, nrow=endrow-startrow+1))
  colnames(tradeAccount)<-c("date","datetime","balanceBegin","buy", "sell", 
                            "holding","balanceEnd", "profit", "totalR")
  tradeAccount[,1]<-jointData$date[startrow:endrow]
  tradeAccount[,2]<-jointData$datetime[startrow:endrow]
  tradeAccount[,3:9]<-0
  # tradeAccount$balanceBegin[1]<-balance0
  shortProfit<-0
  for (i in (1:length(tradeAccount$datetime))){
    if (i==1){
      # tradeAccount$balanceBegin[i]<-balance0
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-0
      # tradeAccount$balanceEnd[i]<-balance0
      # tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$profit[i]<-0
      # tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    } else if(i==length(tradeAccount$datetime)){
      # tradeAccount$balanceEnd[i]<-tradeAccount$balanceBegin[i]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$holding[i-1]
      tradeAccount$profit[i]<-tradeAccount$profit[i-1]
      # tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    } else if (tradeAccount$holding[i-1]>0 && SMA_short[i-1]<=SMA_long[i-1] && SMA_short[i]>=SMA_long[i]){ # buy back
      tradeAccount$buy[i]<-1
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-0
      # tradeAccount$balanceEnd[i]<-0
      # tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$profit[i]<-balance0-tradeAccount$holding[i-1]*jointData2$close[i]+shortProfit
      shortProfit<-balance0-tradeAccount$holding[i-1]*jointData2$close[i]+shortProfit
      # tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
      # buyprice<-jointData2$close[i]
      
    } else if (tradeAccount$holding[i-1]==0 && SMA_short[i-1]>=SMA_long[i-1] && SMA_short[i]<=SMA_long[i]){  # short

      # tradeAccount$balanceEnd[i]<-jointData2$close[i]*tradeAccount$holding[i-1]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-1
      tradeAccount$holding[i]<-balance0/jointData2$close[i]
      # tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$profit[i]<-balance0-tradeAccount$holding[i]*jointData2$close[i]+shortProfit
      # tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
      
    } else if (tradeAccount$holding[i-1]>0){
      # tradeAccount$balanceEnd[i]<-tradeAccount$balanceBegin[i]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$holding[i-1]
      # tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$profit[i]<-balance0-tradeAccount$holding[i-1]*jointData2$close[i]+shortProfit
      # tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    } else {
      # tradeAccount$balanceEnd[i]<-tradeAccount$balanceBegin[i]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$holding[i-1]
      # tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$profit[i]<-shortProfit
      # tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    }
  }
  return(tradeAccount)
} 

tradeGraph <- function(jointData2, tradeAccount, balance0){
  par(
    lty = 1, 		  		  # lty = "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"			  
    lwd = 1,				    # function "lines" takes a vector of values for more than one line, and will cycle through that vector
    pch = ".",				  # point types
    bty = "n",					# suppresses box around the plot, keep part by letters like "c" 
    cex.axis = 1.5, col.axis = grey(.6) 	, # font.axis = ,		#size, color and font of axis text
    xaxt ="s", 
    yaxt ="s",  	 		  # s=standard, n=suppress axis
    
    cex.lab  = 1.5, col.lab = grey(.6),  font.lab  = 6,	 # axis label size, color and font
    cex.main =  1.2, col.main= grey(.4),  font.main = 8, # main label, color and font
    cex.sub  =  1.3, col.sub = grey(.4),  font.sub  = 6, # sub label, color and font
    
    #tick marks
    lab = c(x=10,y=10,len=.1)	, # las = 	,		#number of ticks, las = axis labels 0 = default, 1=horizontal, 2 = perpendicular, 3 = vertical
    tck = -.01  			,         # negative is external ticks, 1 is gridlines
    lend = "square"			,       # tick mark end "round" default, "butt" butt lines, "square" square line caps
    
    fg = grey(.2),	 # foreground color
    bg = grey(.95),
    family= "serif",
    mar=c(8,6,2,4)   # https://nicercode.github.io/guides/plotting/
  )
  
  date_vec<-as.Date(jointData2$datetime)
  time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "quarter"), format="%Y")
  time_label<-character()
  for (i in (1:length(time_label_y))){
    if (i %% 4 == 1){
      time_label[i]<-time_label_y[i]
    } else if (i %% 4 == 2){
      time_label[i]<-"Q2"
    } else if (i %% 4 == 3) {
      time_label[i]<-"Q3"
    } else {
      time_label[i]<-"Q4"
    }
  }
  timeFreq<-"quarter"
  
  plot(as.Date(jointData2$datetime), jointData2$close, type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0,max(jointData2$close)*1.05),
       col.ticks="white", col="red", lwd=1.5)
  
  axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = timeFreq),
            labels= time_label,col="white",tck=1)   
  
  axis(2,col="white",tck=1,col.ticks="white", at=seq(0, max(jointData2$close)*1.05, by =round(max(jointData2$close)*1.05/5,3)))
  title(ylab="Coin Price", line=3, cex.lab=1.5)
  
  par(new=T)
  plot(as.Date(jointData2$datetime), jointData2$SMA_long, type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0,max(jointData2$close)*1.05),
       col.ticks="white", col="dodgerblue3", lwd=1.5)

  # par(new=T)
  # plot(as.Date(jointData2$datetime), jointData2$teleFreq, type="p",
  #      ylab="",
  #      xlab="",
  #      xaxt="n",
  #      yaxt="n",
  #      ylim=c(0,max(jointData2$teleFreq)*1.05),
  #      col.ticks="white", col="deeppink3", lwd=3)
  
  par(new=T)
  plot(as.Date(jointData2$datetime), 
       tradeAccount$buy,
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0.5,1),
       col="forestgreen",
       pch=17) 
  
  par(new=T)
  plot(as.Date(jointData2$datetime), 
       tradeAccount$sell,
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0.5,1),
       col="darkred",
       pch=17) 
  
  par(new=T)
  max_v=max(tradeAccount$total/balance0)
  plot(as.Date(jointData2$datetime), tradeAccount$total/balance0,
       ylim=c(0,max_v), type="l",col="gray50",xaxt="n",yaxt="n",xlab="",ylab="", lwd=1.5)
  axis(4, col=grey(.95), at = seq(0, max_v, by = round(max_v*100/6,0)/100), labels = per_format(seq(0, max_v, by = round(max_v*100/6,0)/100) ))
  
  par(xpd=TRUE)
  coord <- par("usr")
  
  legend(coord[1]+(coord[2]-coord[1])/6, coord[3]-coord[4]/8, col=c("red","dodgerblue3", "gray50", "forestgreen", "darkred"),
         lty=c(1,1, 1,NA,NA),pch=c(NA, NA, NA, 17, 17), 
         legend=c("price","SMA", "total_return", "buy signal", "sell signal"),  
         text.col="gray50", cex=1.5, pt.cex = 1, box.col = "gray95", box.lwd = 0,
         ncol =3, inset = c(0, 0), bty = "n")
}

tradeGraphTele <- function(jointData2, tradeAccount, balance0, tradeShortTotal){
  par(
    lty = 1, 		  		  # lty = "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"			  
    lwd = 1,				    # function "lines" takes a vector of values for more than one line, and will cycle through that vector
    pch = ".",				  # point types
    bty = "n",					# suppresses box around the plot, keep part by letters like "c" 
    cex.axis = 1.5, col.axis = grey(.6) 	, # font.axis = ,		#size, color and font of axis text
    xaxt ="s", 
    yaxt ="s",  	 		  # s=standard, n=suppress axis
    
    cex.lab  = 1.5, col.lab = grey(.6),  font.lab  = 6,	 # axis label size, color and font
    cex.main =  1.2, col.main= grey(.4),  font.main = 8, # main label, color and font
    cex.sub  =  1.3, col.sub = grey(.4),  font.sub  = 6, # sub label, color and font
    
    #tick marks
    lab = c(x=10,y=10,len=.1)	, # las = 	,		#number of ticks, las = axis labels 0 = default, 1=horizontal, 2 = perpendicular, 3 = vertical
    tck = -.01  			,         # negative is external ticks, 1 is gridlines
    lend = "square"			,       # tick mark end "round" default, "butt" butt lines, "square" square line caps
    
    fg = grey(.2),	 # foreground color
    bg = grey(.95),
    family= "serif",
    mar=c(8,6,2,4)   # https://nicercode.github.io/guides/plotting/
  )
  
  date_vec<-as.Date(jointData2$datetime)
  time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "quarter"), format="%Y")
  time_label<-character()
  for (i in (1:length(time_label_y))){
    if (i %% 4 == 1){
      time_label[i]<-time_label_y[i]
    } else if (i %% 4 == 2){
      time_label[i]<-"Q2"
    } else if (i %% 4 == 3) {
      time_label[i]<-"Q3"
    } else {
      time_label[i]<-"Q4"
    }
  }
  timeFreq<-"quarter"
  
  plot(as.Date(jointData2$datetime), jointData2$close, type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0,max(jointData2$close)*1.05),
       col.ticks="white", col="red", lwd=1.5)
  
  axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = timeFreq),
            labels= time_label,col="white",tck=1)   
  
  axis(2,col="white",tck=1,col.ticks="white", at=seq(0, max(jointData2$close)*1.05, by =round(max(jointData2$close)*1.05/5,3)))
  title(ylab="Coin Price", line=3, cex.lab=1.5)
  
  par(new=T)
  plot(as.Date(jointData2$datetime), jointData2$SMA_long, type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0,max(jointData2$close)*1.05),
       col.ticks="white", col="dodgerblue3", lwd=1.5)
  
  # par(new=T)
  # plot(as.Date(jointData2$datetime), jointData2$teleFreq, type="p",
  #      ylab="",
  #      xlab="",
  #      xaxt="n",
  #      yaxt="n",
  #      ylim=c(0,max(jointData2$teleFreq)*1.05),
  #      col.ticks="white", col="deeppink3", lwd=3)
  
  par(new=T)
  plot(as.Date(jointData2$datetime), 
       tradeAccount$buy,
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0.5,1),
       col="forestgreen",
       pch=17) 
  
  par(new=T)
  plot(as.Date(jointData2$datetime), 
       tradeAccount$sell,
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0.5,1),
       col="darkred",
       pch=17) 
  
  par(new=T)
  max_v=max(tradeAccount$total/balance0, tradeShortTotal/balance0)
  plot(as.Date(jointData2$datetime), tradeAccount$total/balance0,
       ylim=c(0,max_v), type="l",col="gray50",xaxt="n",yaxt="n",xlab="",ylab="", lwd=1.5)
  axis(4, col=grey(.95), at = seq(0, max_v, by = round(max_v*100/6,0)/100), labels = per_format(seq(0, max_v, by = round(max_v*100/6,0)/100) ))
  par(new=T)
  plot(as.Date(jointData2$datetime), tradeShortTotal/balance0,
       ylim=c(0,max_v), type="l",col="darkorchid",xaxt="n",yaxt="n",xlab="",ylab="", lwd=1.5)
  axis(4, col=grey(.95), at = seq(0, max_v, by = round(max_v*100/6,0)/100), labels = per_format(seq(0, max_v, by = round(max_v*100/6,0)/100) ))
  
  
  par(xpd=TRUE)
  coord <- par("usr")
  
  legend(coord[1]+(coord[2]-coord[1])/6, coord[3]-coord[4]/8, col=c("red","dodgerblue3", "gray50", "darkorchid", "forestgreen", "darkred"),
         lty=c(1, 1, 1, 1, NA, NA),pch=c(NA, NA, NA, NA, 17, 17), 
         legend=c("price","SMA", "total_return", "total_tele","buy signal", "sell signal"),  
         text.col="gray50", cex=1.5, pt.cex = 1, box.col = "gray95", box.lwd = 0,
         ncol =3, inset = c(0, 0), bty = "n")
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


tradeGraphShort <- function(jointData2, tradeAccount, balance0, tradeShortTotal){
  par(
    lty = 1, 		  		  # lty = "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"			  
    lwd = 1,				    # function "lines" takes a vector of values for more than one line, and will cycle through that vector
    pch = ".",				  # point types
    bty = "n",					# suppresses box around the plot, keep part by letters like "c" 
    cex.axis = 1.5, col.axis = grey(.6) 	, # font.axis = ,		#size, color and font of axis text
    xaxt ="s", 
    yaxt ="s",  	 		  # s=standard, n=suppress axis
    
    cex.lab  = 1.5, col.lab = grey(.6),  font.lab  = 6,	 # axis label size, color and font
    cex.main =  1.2, col.main= grey(.4),  font.main = 8, # main label, color and font
    cex.sub  =  1.3, col.sub = grey(.4),  font.sub  = 6, # sub label, color and font
    
    #tick marks
    lab = c(x=10,y=10,len=.1)	, # las = 	,		#number of ticks, las = axis labels 0 = default, 1=horizontal, 2 = perpendicular, 3 = vertical
    tck = -.01  			,         # negative is external ticks, 1 is gridlines
    lend = "square"			,       # tick mark end "round" default, "butt" butt lines, "square" square line caps
    
    fg = grey(.2),	 # foreground color
    bg = grey(.95),
    family= "serif",
    mar=c(8,6,2,4)   # https://nicercode.github.io/guides/plotting/
  )
  
  date_vec<-as.Date(jointData2$datetime)
  time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "quarter"), format="%Y")
  time_label<-character()
  for (i in (1:length(time_label_y))){
    if (i %% 4 == 1){
      time_label[i]<-time_label_y[i]
    } else if (i %% 4 == 2){
      time_label[i]<-"Q2"
    } else if (i %% 4 == 3) {
      time_label[i]<-"Q3"
    } else {
      time_label[i]<-"Q4"
    }
  }
  timeFreq<-"quarter"
  
  plot(as.Date(jointData2$datetime), jointData2$close, type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0,max(jointData2$close)*1.05),
       col.ticks="white", col="red", lwd=1.5)
  
  axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = timeFreq),
            labels= time_label,col="white",tck=1)   
  
  axis(2,col="white",tck=1,col.ticks="white", at=seq(0, max(jointData2$close)*1.05, by =round(max(jointData2$close)*1.05/5,3)))
  title(ylab="Coin Price", line=3, cex.lab=1.5)
  
  par(new=T)
  plot(as.Date(jointData2$datetime), jointData2$SMA_long, type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0,max(jointData2$close)*1.05),
       col.ticks="white", col="dodgerblue3", lwd=1.5)
  
  # par(new=T)
  # plot(as.Date(jointData2$datetime), jointData2$teleFreq, type="p",
  #      ylab="",
  #      xlab="",
  #      xaxt="n",
  #      yaxt="n",
  #      ylim=c(0,max(jointData2$teleFreq)*1.05),
  #      col.ticks="white", col="deeppink3", lwd=3)
  
  par(new=T)
  plot(as.Date(jointData2$datetime), 
       tradeAccount$buy,
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0.5,1),
       col="forestgreen",
       pch=17) 
  
  par(new=T)
  plot(as.Date(jointData2$datetime), 
       tradeAccount$sell,
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0.5,1),
       col="darkred",
       pch=17) 
  
  par(new=T)
  max_v=max(tradeAccount$total/balance0, tradeShortTotal/balance0)
  plot(as.Date(jointData2$datetime), tradeAccount$total/balance0,
       ylim=c(0,max_v), type="l",col="gray50",xaxt="n",yaxt="n",xlab="",ylab="", lwd=1.5)
  axis(4, col=grey(.95), at = seq(0, max_v, by = round(max_v*100/6,0)/100), labels = per_format(seq(0, max_v, by = round(max_v*100/6,0)/100) ))
  par(new=T)
  plot(as.Date(jointData2$datetime), tradeShortTotal/balance0,
       ylim=c(0,max_v), type="l",col="darkorchid",xaxt="n",yaxt="n",xlab="",ylab="", lwd=1.5)
  axis(4, col=grey(.95), at = seq(0, max_v, by = round(max_v*100/6,0)/100), labels = per_format(seq(0, max_v, by = round(max_v*100/6,0)/100) ))
  
  
  par(xpd=TRUE)
  coord <- par("usr")
  
  legend(coord[1]+(coord[2]-coord[1])/6, coord[3]-coord[4]/8, col=c("red","dodgerblue3", "gray50", "darkorchid", "forestgreen", "darkred"),
         lty=c(1, 1, 1, 1, NA, NA),pch=c(NA, NA, NA, NA, 17, 17), 
         legend=c("price","SMA", "total_return", "total_shorting","buy signal", "sell signal"),  
         text.col="gray50", cex=1.5, pt.cex = 1, box.col = "gray95", box.lwd = 0,
         ncol =3, inset = c(0, 0), bty = "n")
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


