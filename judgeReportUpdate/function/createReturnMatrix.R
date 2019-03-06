createReturnMatrix<-function(dfname){
  datetemp<- eval(parse(text = paste0(dfname, "$date")))[-1]
  datatemp <- diff(log(eval(parse(text = paste0(dfname, "$close")))), lag=1)
  datatemp[which(is.infinite(datatemp))] <- NA
  
  # combine datatemp
  datatemp<-cbind(datetemp, datatemp)
  datatemp<-as.data.frame(datatemp)
  return(datatemp)
}