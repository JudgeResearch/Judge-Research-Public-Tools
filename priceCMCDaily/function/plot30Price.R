# plot 30 price in one chart
plot30Price<-function(date_vec, time_label, data_p, tokenList, startIndex, endIndex){
  # par options for plot function
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
  max_vec=rep(0,30)
  for (i in 1:30){
    data1_p<-data_p[startIndex:endIndex,i+1]
    price_vec<-data1_p*(1/data1_p[max(which(data1_p>0)[1],1)])
    max_vec[i]<-max(price_vec, na.rm = TRUE)
  }
  
  max_max_vec <- max(max_vec)
  color_vec=character()
  
  data1_p<-data_p[startIndex:endIndex,2]
  price_vec<-data1_p*(1/data1_p[max(which(data1_p>0)[1],1)])
  
  plot(as.Date(date_vec), price_vec, type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0, 3),
       col.ticks="white", col="gray50", lwd=0.25)
  axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = "month"),
            labels= time_label,col="white",tck=1)   
  # strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b-%y")
  axis(2,col="white",tck=1,col.ticks="white", at = seq(0, 3, by = 0.5*ceiling(3/600)))
  if (startIndex<790){
    title(ylab="Jan 1, 2018 = 1", line=3, cex.lab=1.5)
  } else {
    title(ylab="Mar 1, 2018 = 1", line=3, cex.lab=1.5)
  }
  
  color_vec[1]<-"gray50"
  
  for (i in (1:1)){
    for (j in (2:30)){
      data1_p<-data_p[startIndex:endIndex,j+1]
      price_vec<-data1_p*(1/data1_p[max(which(data1_p>0)[1],1)])
      
      par(new=T)
      plot(as.Date(date_vec), price_vec, ylim=c(0, 3), type="l",col=colors()[j+10],
           xaxt="n",yaxt="n",xlab="",ylab="", lwd=0.25, ann=FALSE)
      # https://stackoverflow.com/questions/11775692/how-to-specify-the-actual-x-axis-values-to-plot-as-x-axis-ticks-in-r
      # axis(4, col=grey(.95), at = seq(0, max_v, by = 10*ceiling(max_v/70)))
      color_vec[j]<-colors()[j+10]
    }
  }    
  par(xpd=TRUE)
  coord <- par("usr")
  
  # add legend to the chart
  legend(coord[1]+20, coord[3]-0.3, col=color_vec,lty=1,
         legend=tokenList$fsym[1:30],  
         text.col="gray50", cex=1.2, pt.cex = 1, box.col = "gray95", box.lwd = 0,
         ncol =8, inset = c(0, 0), bty = "n")
}