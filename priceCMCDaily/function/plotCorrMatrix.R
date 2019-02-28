# plot Token Correlation Matrix
plotCorrMatrix<-function(cormat){
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
  
  melted_cormat <- melt(cormat)
  ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value, label=value)) + 
    geom_tile() +
    geom_text(size = 4, color="white") +
    xlab("") +
    ylab("") +
    scale_fill_gradient2(low = "gray100", high = "gray40", mid = "gray70", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation")
}