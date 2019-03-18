#---------------------------------------------------------------------------------
#
# NAME:
#       plotDccdata.R
#
# PURPOSE:
#       plot 2 DCC data series with BTC price
#
# CATEGORY:
#       Data Visualization
#
# CALLING SEQUENCE:
#       plotDccdata(date_vec, time_label, y_label, timeFreq, dcc_vec, data1_p, data2_p, coinSummary_2, i, j){
#       
# REQUIRED INPUTS:
#     ___
#     (1) date_vec
#         data used by x-axis
#     ___
#     (2) time_label
#         label used for charting
#     __
#     (3) y_label
#         text label used for charting
#     __
#     (4) timeFreq
#         frequency for charting
#     __
#     (5) dcc_vec
#         data DCC vector
#     __
#     (6) data1_p
#         data price vecctor
#     __
#     (7) data2_p
#         data price vector 
#     __
#     (8) coinSummary_2
#         reference for ticker name and base
#
# OPTIONAL INPUTS:
#     ___
#     (1) none
#         
# KEYWORD PARAMETERS:
#     none
#
# OUTPUTS:
#     (1) none
#     (2) 
#     (3) 
#
# OPTIONAL OUTPUTS:
#     none
# 
# RESTRICTIONS:
#     (1) 
#
# REQUIREMENTS:
#     (1) none
#     (2) 
#     (3) 
#
# REFERENCES:       
#
# Next Steps (If not done) :
#     This script can be enhanced as follows:
#     (1) none
#     (2) 
#     (3) 
#
#
#----------------------------------------------------------------------------------
plotDccdata<-function(date_vec, time_label, y_label, timeFreq, dcc_vec, data1_p, data2_p, coinSummary_2, i, j){
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
    mar=c(6,6,2,4)   # https://nicercode.github.io/guides/plotting/
  )
  
  plot(as.Date(date_vec), dcc_vec, type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(-1,1),
       col.ticks="white", col="red", lwd=1.5)
  
  axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = timeFreq),
            labels= time_label,col="white",tck=1)   
  
  axis(2,col="white",tck=1,col.ticks="white", at=seq(-1, 1, by =0.2), labels = numformat(y_label))
  title(ylab="DCC Estimate", line=3, cex.lab=1.5)
  
  par(new=T)
  max_v<-max(max(data1_p, na.rm=TRUE),max(data2_p, na.rm=TRUE))
  plot(as.Date(date_vec), data1_p, ylim=c(0,max_v), type="l",col="gray50",xaxt="n",yaxt="n",xlab="",ylab="")
  # https://stackoverflow.com/questions/11775692/how-to-specify-the-actual-x-axis-values-to-plot-as-x-axis-ticks-in-r
  axis(4, col=grey(.95), at = seq(0, max_v, by = round(max_v/6,2)))
  par(new=T)
  plot(as.Date(date_vec), data2_p,  ylim=c(0,max_v),type="l",col="lightblue3",xaxt="n",yaxt="n",xlab="",ylab="")
  axis(4, col=grey(.95), at = seq(0, max_v, by = round(max_v/6,2)))
  
  par(new=TRUE) 
  plot(as.Date(date_vec), dcc_vec, type="l",
       xaxt="n",
       ylim=c(-1, 1),
       col="red", lwd=1.5, axes = FALSE, xlab = "", ylab = "")
  
  par(xpd=TRUE)
  coord <- par("usr")
  
  legend(coord[1]+(coord[2]-coord[1])/3, coord[3]-coord[4]/4, col=c("red","gray50", "lightblue3"),lty=1,
         legend=c("DCC",paste(coinSummary_2[i]), paste(coinSummary_2[j])),  
         text.col="gray50", cex=1.5, pt.cex = 1, box.col = "gray95", box.lwd = 0,
         ncol =8, inset = c(0, 0), bty = "n")
  
  # legend(as.Date(date_vec)[1],-0.58,col=c("red","gray50", "lightblue3"),lty=1,
  #        legend=c("DCC",paste(coinSummary[i]), paste(coinSummary[j])), 
  #        text.col="gray50", cex=1.5, pt.cex = 1, box.col = "gray95", box.lwd = 0)
}