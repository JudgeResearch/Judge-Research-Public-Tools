createDccData<-function(x_r, coinSummary, i, j){
  # generate the data for dcc analysis
  data1<-eval(parse(text=paste0("x_r$",coinSummary[i,1])))
  data1<-data1[paste(startdate,enddate, sep="/"), colnames(data1)]
  twindow<-coinSummary[i,8]
  if(coinSummary[i,4]>(twindow+100)) {
    data1<-(data1[(1+twindow):length(data1)])
  }
  data2<-eval(parse(text=paste0("x_r$",coinSummary[j,1])))
  data2<-data2[paste(startdate,enddate, sep="/"), colnames(data2)]
  if(coinSummary[j,4]>(twindow+100)) {
    data2<-(data2[(1+twindow):length(data2)])
  }
  x12 <- merge(data1, data2)
  tst <- na.approx(na.trim(x12, side="both"))
  return(tst)
}