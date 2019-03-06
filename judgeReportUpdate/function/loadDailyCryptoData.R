loadDailyCryptoData <- function(folderCmc, fileName, tokenList, i) {
  datatemp<-read.table(file.path(folderCmc, fileName), header=TRUE, sep=",", stringsAsFactors = FALSE, comment.char = "")
  
  datatemp$fsym<-tokenList[i,2]
  datatemp$tsym<-tokenList[i,3]
  datatemp<-datatemp[,c(1,8:9,2:6)]
  
  colnames(datatemp)<-c("date","fsym","tsym","open","high","low","close","vol.f")
  datatemp [datatemp==0]<-NA
  
  return(datatemp)
}