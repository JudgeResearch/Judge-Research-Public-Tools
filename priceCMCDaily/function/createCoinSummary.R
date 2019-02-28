createCoinSummary <- function(zz, tokenList, twindow) {
  coinSummary[1]<-paste0(tokenList$fsym[i],"_",tokenList$tsym[i])
  # https://stackoverflow.com/questions/6465222/access-zoo-or-xts-index
  coinSummary[2]<-as.character(as.Date(index(zz)[1]))
  coinSummary[3]<-as.character(as.Date(index(zz)[length(zz)]))
  coinSummary[4]<-length(zz)
  if(coinSummary[4]>(twindow+100)) {
    zzz<-auto.arima(zz[(1+twindow):length(zz)])
  } else {
    zzz<-auto.arima(zz[1:length(zz)])
  }
  coinSummary[5:7]<-rbind(arimaorder(zzz))
  coinSummary[8]<-if(coinSummary[i,4]>(twindow+100)) twindow else 0
  return(coinSummary)
}