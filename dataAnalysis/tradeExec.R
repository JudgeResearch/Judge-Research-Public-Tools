tradeExec <- function(startrow, endrow, balance0, SMA_short, SMA_long, jointData2){
  
  # SMA_short<-SMA_short
  # SMA_long<-SMA_long
  tradeAccount<- as.data.frame(matrix(ncol=9, nrow=endrow-startrow+1))
  colnames(tradeAccount)<-c("date","datetime","balanceBegin","buy", "sell", "holding","balanceEnd", "total", "totalR")
  tradeAccount[,1]<-jointData$date[startrow:endrow]
  tradeAccount[,2]<-jointData$datetime[startrow:endrow]
  tradeAccount[,3:9]<-0
  tradeAccount$balanceBegin[1]<-balance0
  for (i in (1:length(tradeAccount$datetime))){
    # for (i in (1:700)){ 
    # && jointData2$teleFreq[i]>buysignal 
    if (i==1){
      tradeAccount$balanceBegin[i]<-balance0
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-0
      tradeAccount$balanceEnd[i]<-balance0
      tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    } else if(i==length(tradeAccount$datetime)){
      tradeAccount$balanceEnd[i]<-tradeAccount$balanceBegin[i]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$holding[i-1]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    } else if (tradeAccount$balanceBegin[i]>0 
               && SMA_short[i-1]<=SMA_long[i-1] && SMA_short[i]>=SMA_long[i]){
      tradeAccount$buy[i]<-1
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$balanceBegin[i]/jointData2$close[i]
      tradeAccount$balanceEnd[i]<-0
      tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
      buyprice<-jointData2$close[i]
      
    } else if (tradeAccount$holding[i-1]>0 && SMA_short[i-1]>=SMA_long[i-1] && SMA_short[i]<=SMA_long[i]){
      # && jointData2$rsiMA1[i]>rsiSell) && jointData2$close[i]>buyprice*minR  {
      tradeAccount$balanceEnd[i]<-jointData2$close[i]*tradeAccount$holding[i-1]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-1
      tradeAccount$holding[i]<-0
      tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
      
    } else {
      tradeAccount$balanceEnd[i]<-tradeAccount$balanceBegin[i]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$holding[i-1]
      tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$total[i]<-tradeAccount$balanceEnd[i]+tradeAccount$holding[i]*jointData2$close[i]
      tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    }
  }
  return(tradeAccount)
}