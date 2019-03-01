# =======================================================
# create data_p matrix
i<-1
data_p <- as.data.frame(BTC_USD$date[-1]) # to keep the data length the same as return
colnames(data_p)[1]<-"date"
for (i in (1:length(cmc_coin$coin))){
  dfname<-paste0(cmc_coin$coin[i], "_USD")
  datatemp<-createPriceMatrix(dfname) # createPriceMatrix function
  
  # join the data based on date
  data_p <- sqldf(paste0("select a.*, b.datatemp as ", 
                         paste0(dfname, "_p"), " from data_p a left join datatemp", 
                         " b on a.date=b.datetemp"))
  progress <- i/length(cmc_coin$coin)
  message("Calculate price of ",cmc_coin$coin[i], "_USD", " | Progress ", paste(round(100*progress, 2), "%", sep=""))
}
save(data_p, file="./rdata/data_p.rdata")


# =====================================================
# Create data_r matrix
i<-1
data_r <- as.data.frame(BTC_USD$date[-1]) 
colnames(data_r)[1] <- "date"
for (i in (1:length(cmc_coin$coin))){
  
  dfname  <-paste0(cmc_coin$coin[i], "_USD")
  datatemp<-createReturnMatrix(dfname) # create return matrix function
  
  # join the data based on date
  data_r  <- sqldf(paste0("select a.*, b.datatemp as ", 
                         paste0(dfname, "_r"), " from data_r a left join datatemp", 
                         " b on a.date=b.datetemp"))    
  progress<- i/length(cmc_coin$coin)
  message("Calculate return of ",cmc_coin$coin[i], "_USD", " | Progress ", paste(round(100*progress, 2), "%", sep="") )
}
save(data_r, file="./radata/data_r.rdata")

# =======================================================
# create coin summary table
date_r       <- data_r[,1]
data_r[, -1] <- as.numeric(unlist(data_r[,-1]))
x_r<-xts(data_r[,-1], order.by=as.Date(date_r, format = "%m/%d/%Y"))

coinSummary  <- as.data.frame(matrix(ncol=4 , nrow=length(cmc_coin$coin)))
colnames(coinSummary)=c("coin", "ar", "i", "ma")
i<-3
for (i in (1:length(cmc_coin$coin))){
  
  zz <- eval(parse(text=paste0("x_r$",colnames(x_r[,i]))))
  zz <- zz[paste(startdate,enddate, sep="/"), colnames(zz)]
  zz <- na.approx(na.trim(zz, side="both"))
  zzz<- auto.arima(zz)
  coinSummary[i,1]  <- paste0(cmc_coin$coin[i],"_USD")
  coinSummary[i,2:4]<- rbind(arimaorder(zzz))
  progress          <- i/length(cmc_coin$coin)
  message("Progress ", paste(round(100*progress, 2), "%", sep="") )
}
save(coinSummary, file="./rdata/coinSummary.rdata")

