#---------------------------------------------------------------------------------
#
# NAME:
#       plot30DCC.R
#
# PURPOSE:
#       Plot multiple dcc in one chart
#
# CATEGORY:
#       DataVisualization
#
# CALLING SEQUENCE:
#       plot30Dcc(date_vec, time_label, y_label, data_dcc, data_p, tokenList)
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
#     (4) data_dcc
#         dataframe of DCC data
#     __
#     (5) data_p
#         dataframe of price data
#     __
#     (6) tokenList
#         dataframe of tokens for loop reference
#
# OPTIONAL INPUTS: 
#     none
#   
# KEYWORD PARAMETERS:
#     none
#
# OUTPUTS:
#     (1) plot multiple DCC data series in one chart
#
# OPTIONAL OUTPUTS:
#     none
# 
# RESTRICTIONS:
#     (1)
#
# REQUIREMENTS:
#     (1)
#     (2) 
#     (3) 
#
# REFERENCES:       
#
# Next Steps (If not done) :
#     This script can be enhanced as follows:
#     (1) 
#     (2) 
#     (3) 
#
#
#----------------------------------------------------------------------------------






plot30Dcc<-function(date_vec, time_label, y_label, data_dcc, data_p, tokenList){
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
    mar=c(10,6,2,4)
  )
  # create a vector for color
  color_vec=character()
  plot(as.Date(date_vec), data_dcc[,2], type="l",
       ylab="",
       xlab="",
       xaxt="n",
       yaxt="n",
       ylim=c(-1,1),
       col.ticks="white", col="gray50", lwd=0.25)
  axis.Date(1, at = seq(date_vec[1], date_vec[length(date_vec)], by = "quarter"),
            labels= time_label,col="white",tck=1)   
  axis(2,col="white",tck=1,col.ticks="white", at=seq(-1, 1, by =0.2), labels = numformat(y_label))
  title(ylab="DCC Estimate", line=3, cex.lab=1.5)
  
  color_vec[1]<-"gray50"
  i=1
  j=1
  
  vec_length<-length(data_dcc[,1])
  
  for (i in (1:1)){
    for (j in (2:30)){
      # create xts object for plot
      if (i==j){
        dcc_vec <- rep(1,vec_length)
      } else if(j>i) {
        dcc_vec <- eval(parse(text = paste0("data_dcc$", colnames(data_r)[i+1],"_", colnames(data_r)[j+1])))
      } else {
        dcc_vec <- eval(parse(text = paste0("data_dcc$", colnames(data_r)[j+1],"_", colnames(data_r)[i+1])))
      }
      
      par(new=T)
      plot(as.Date(date_vec), dcc_vec, ylim=c(-1, 1), type="l",col=colors()[j+10],xaxt="n",yaxt="n",xlab="",ylab="", lwd=0.25, ann=FALSE)
      # https://stackoverflow.com/questions/11775692/how-to-specify-the-actual-x-axis-values-to-plot-as-x-axis-ticks-in-r
      color_vec[j]<-colors()[j+10]
    }
  }    
  
  data1_p<-eval(parse(text=paste0("data_p$BTC_USD_p")))
  data1_p<-data1_p*(1/data1_p[max(which(data1_p>0)[1],424)]) # use 3/1/2017 as baseline if it is higher
  
  par(new=T)
  max_v<-max(max(data1_p, na.rm=TRUE),max(data1_p, na.rm=TRUE))
  # bring the price chart to front to distinguish with other 30 DCC series. 
  plot(as.Date(date_vec), data1_p,  ylim=c(0,max_v),type="l",col="gray30",xaxt="n",yaxt="n",xlab="",ylab="", lwd=1)
  axis(4, col=grey(.95), at = seq(0, max_v, by = 5*ceiling(max_v/70)))
  
  par(xpd=TRUE)
  coord <- par("usr")
  
  # add legend to the chart
  legend(coord[1]+20, coord[3]-2, col=color_vec,lty=1,
         legend=tokenList$fsym[1:30],  
         text.col="gray50", cex=1.2, pt.cex = 1, box.col = "gray95", box.lwd = 0,
         ncol =8, inset = c(0, 0), bty = "n")
}