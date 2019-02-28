createPriceMatrix<-function(dfname){
  datetemp<- eval(parse(text = paste0(dfname, "$date")))
  datetemp<-datetemp
  datatemp <- eval(parse(text = paste0(dfname, "$close")))
  datatemp[which(is.infinite(datatemp))] <- NA
  datatemp<-cbind(datetemp, datatemp)
  datatemp<-as.data.frame(datatemp)
  return(datatemp)
}