# ignore the warning about "col.ticks."  it works fine.  pass along whatever pars you want.  
# alter settings if you  want different defaults

judgeplot <- function (x=y, y=x, type="ts", ..., byord = FALSE )		     {
if ( all(x == y, na.rm = TRUE) == TRUE & byord == FALSE & type != "density" & type != "point" & type != "hist") type <- "ts"
par(
lty = 1, 		  		  # lty = "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash"			  
lwd = 1,				  # function "lines" takes a vector of values for more than one line, and will cycle through that vector
pch = ".",				  # point types
bty = "n",								#suppresses box around the plot, keep part by letters like "c" 
cex.axis = .7, col.axis = grey(.6) 	, # font.axis = ,		#size, color and font of axis text
xaxt="s", yaxt ="s",	 		  # s=standard, n=suppress axis

cex.lab  = .9, col.lab = grey(.6),  font.lab  = 6,	 #axis label size, color and font
cex.main =  1, col.main= grey(.4),  font.main = 8,		#main label, color and font
cex.sub  =  1, col.sub = grey(.4),  font.sub  = 6,		#sub label, color and font

#tick marks
lab = c(x=10,y=10,len=.1)	, # las = 	,		#number of ticks, las = axis labels 0 = default, 1=horizontal, 2 = perpendicular, 3 = vertical
tck = -.01  			, # negative is external ticks, 1 is gridlines
lend = "square"			, # tick mark end "round" default, "butt" butt lines, "square" square line caps
#tcl  = .5 				, # ratio of tick size to text 
	  				  # xaxs, yax
					  # sets tick marks at intervals "r",i,e,s,d are the options 
# mai 				  # num vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
fg = grey(.2),	      	  # foreground color
bg = grey(.95)
)

if (type != "hist" & type != "ts" & type != "density")  plot(x, y, col.ticks="white",col="red", ...)
if (type == "ts") {   x <- 1:length(y);  plot(x, y, col.ticks="white", col="red", ...)  }
if (type == "density") {dens <- density(x, na.rm=TRUE); plot(dens, col="red", col.ticks="white", ...) }
if (type == "hist")  hist(x, freq=FALSE, col=grey(.87), col.ticks="white", border=grey(.87), ...)
axis(1,col="white",tck=1)
axis(2,col="white",tck=1,col.ticks="white")
if (type == "hist")  hist(x, freq=FALSE, col=grey(.87), border=grey(.87),col.ticks="white",...,add=TRUE )
if (type == "point") points(x,y,col="red", ...)
if (type == "line"  | type == "ts")  lines (x,y,col="red", ...)
if (type == "density") { lines(dens, col="red", ...) }

}

#below three lines how to do full distribution graph
s = rnorm(40)
ss<-c(s-1.5,s+1.5)
judgeplot(x=ss,type="ts",main="fig. 2a. likelihood function of a series with a large break in the conditional mean",ylim=c(-10,10))
# points(d,dd,col="black")
# points(d+1.5,dd,col="black")
# lines(density(ss),col="red")
# sss<-c(s+.4,s-.4,s-1.5,s+1.5,s,s+1,s-1,s-.5,s+.25,s-.25)
# judgeplot(x=sss,type="hist",main="fig. 2b. likelihood function of a series with ten large break in the conditional mean",ylim=c(0,.45))
# points(d+.4,dd,col="black"); points(d-.4,dd,col="black"); points(d+1.5,dd,col="black")
# points(d+1.5,dd,col="black");points(d,dd,col="black");points(d+1,dd,col="black")
# points(d-1,dd,col="black");points(d+.5,dd,col="black");points(d-.25,dd,col="black")
# points(d+.25,dd,col="black")
# lines(density(sss),col="red",lwd=3)
# 
# judgeplot(garchdat[,19])
# 
# # main = main label, ylab,xlab, axis labels, col  put in given plots
# # layout and split.screen for multiple graphs
# d <-   seq(-4,4,length=2000)
# normd <-  1/sqrt(2*pi)*exp(-d^2/2)


