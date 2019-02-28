plotBnbPriceVolume<-function(date_vec, time_label, BNB_USD){
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
    mar=c(4,6,4,4)
  )
  
  # plot the chart
  max_v=max(BNB_USD$close)
  y_label<- seq(0, max_v, by = 5*ceiling(max_v/60))
  
  plot(as.Date(date_vec), BNB_USD$close, type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(0, max_v),
       col.ticks="white", col="red", lwd=1.5)
  # http://r.789695.n4.nabble.com/plot-dates-in-x-axis-td875997.html
  axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = "quarter"),
            labels= time_label,col="white",tck=1)   
  axis(2,col="white",tck=1,col.ticks="white", at=seq(0, max_v, by = 5*ceiling(max_v/60)), labels = numformat(y_label))
  title(ylab="BNB Price", line=3, cex.lab=1.5)
  
  par(new=T)
  barplot(BNB_USD$vol.f, beside=TRUE, yaxt="n", ylim = c(0,10^9))
  max_vol<-max(BNB_USD$vol.f)
}