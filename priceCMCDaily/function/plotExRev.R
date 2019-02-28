# plot Exchange Revenue
plotExRev<-function(exDailyRev){
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
    mar=c(10,7,2,4)
  )
  
  # plot the chart
  max_v=ceiling(max(exDailyRev$daily_rev)/10^6)*10^6
  y_label<- seq(0, max_v, by = max_v/4)
  
  barplot(exDailyRev$daily_rev, width = 5, space = NULL, xlab="", ylab="",
          names.arg = exDailyRev$exchange, las=2, border=0, yaxt="n",
          horiz= FALSE, beside=FALSE,  ylim = c(0,4*10^6))
  axis(2,col="white",tck=1,col.ticks="white", at=y_label, labels = numformat(y_label/10^6), las=2)
  title(ylab="Daily Revenue", line=5, cex.lab=1.5) 
}
