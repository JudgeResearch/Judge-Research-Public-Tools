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