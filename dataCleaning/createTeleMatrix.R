createTeleMatrix<-function(dfname){
  datetemp<- as.POSIXct(eval(parse(text = paste0(dfname, "$datetime"))),origin="1970-01-01",tz="GMT")
  datetemp<-datetemp
  datatemp <- eval(parse(text = paste0(dfname, "$teleFreq")))
  datatemp[which(is.infinite(datatemp))] <- NA
  datatemp<-cbind(datetemp, datatemp)
  datatemp<-as.data.frame(datatemp)
  return(datatemp)
}
