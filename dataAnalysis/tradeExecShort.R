tradeExecShort<-function(startrow, endrow, balance0, SMA_short, SMA_long, jointData2){
  # SMA_short<-jointData2$SMA_short
  # SMA_long<-jointData2$SMA_long
  tradeAccount<- as.data.frame(matrix(ncol=9, nrow=endrow-startrow+1))
  colnames(tradeAccount)<-c("date","datetime","balanceBegin","buy", "sell", 
                            "holding","balanceEnd", "profit", "totalR")
  tradeAccount[,1]<-jointData$date[startrow:endrow]
  tradeAccount[,2]<-jointData$datetime[startrow:endrow]
  tradeAccount[,3:9]<-0
  # tradeAccount$balanceBegin[1]<-balance0
  shortProfit<-0
  for (i in (1:length(tradeAccount$datetime))){
    if (i==1){
      # tradeAccount$balanceBegin[i]<-balance0
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-0
      # tradeAccount$balanceEnd[i]<-balance0
      # tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$profit[i]<-0
      # tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    } else if(i==length(tradeAccount$datetime)){
      # tradeAccount$balanceEnd[i]<-tradeAccount$balanceBegin[i]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$holding[i-1]
      tradeAccount$profit[i]<-tradeAccount$profit[i-1]
      # tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    } else if (tradeAccount$holding[i-1]>0 && SMA_short[i-1]<=SMA_long[i-1] && SMA_short[i]>=SMA_long[i]){ # buy back
      tradeAccount$buy[i]<-1
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-0
      # tradeAccount$balanceEnd[i]<-0
      # tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$profit[i]<-balance0-tradeAccount$holding[i-1]*jointData2$close[i]+shortProfit
      shortProfit<-balance0-tradeAccount$holding[i-1]*jointData2$close[i]+shortProfit
      # tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
      # buyprice<-jointData2$close[i]
      
    } else if (tradeAccount$holding[i-1]==0 && SMA_short[i-1]>=SMA_long[i-1] && SMA_short[i]<=SMA_long[i]){  # short
      
      # tradeAccount$balanceEnd[i]<-jointData2$close[i]*tradeAccount$holding[i-1]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-1
      tradeAccount$holding[i]<-balance0/jointData2$close[i]
      # tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$profit[i]<-balance0-tradeAccount$holding[i]*jointData2$close[i]+shortProfit
      # tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
      
    } else if (tradeAccount$holding[i-1]>0){
      # tradeAccount$balanceEnd[i]<-tradeAccount$balanceBegin[i]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$holding[i-1]
      # tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$profit[i]<-balance0-tradeAccount$holding[i-1]*jointData2$close[i]+shortProfit
      # tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    } else {
      # tradeAccount$balanceEnd[i]<-tradeAccount$balanceBegin[i]
      tradeAccount$buy[i]<-0
      tradeAccount$sell[i]<-0
      tradeAccount$holding[i]<-tradeAccount$holding[i-1]
      # tradeAccount$balanceBegin[i+1]<-tradeAccount$balanceEnd[i]
      tradeAccount$profit[i]<-shortProfit
      # tradeAccount$totalR[i]<-tradeAccount$total[i]/balance0-1
    }
  }
  return(tradeAccount)
} 