loadSp500Data<-function(folderSp, sp500DataFile){
  datatemp<-read.table(file.path(folderSp,sp500DataFile), header=TRUE, sep=",", stringsAsFactors = FALSE, comment.char = "")
  datatemp$fsym<-"SP500"
  datatemp$tsym<-"Index"
  datatemp<-datatemp[,c(1,8:9, 3:5,2,6:7)]
  colnames(datatemp)<-c("date","fsym","tsym","open","high","low","close","vol.f", "sp500_r")
  return(datatemp)
}