estimateDCCMatrix<-function(tst, coinSummary, i, j){
  garch_1_d.spec = ugarchspec(mean.model = list(armaOrder = c(min(1,max(coinSummary[i,5],0)),coinSummary[i,7])),
                              variance.model = list(garchOrder = c(1,1),
                                                    model = "sGARCH"), distribution.model = "norm")
  garch_2_d.spec = ugarchspec(mean.model = list(armaOrder = c(min(1,max(coinSummary[j,5],0)),coinSummary[j,7])),
                              variance.model = list(garchOrder = c(1,1),
                                                    model = "sGARCH"), distribution.model = "norm")
  # dcc specification - GARCH(1,1) for conditional correlations
  dcc.spec <-multispec( list(garch_1_d.spec, garch_2_d.spec) )
  dcc.garch12_d.spec = dccspec(uspec = dcc.spec, dccOrder = c(1,1), distribution = "mvnorm")
  
  # run dcc-garch model
  dcc.fit <- dccfit(dcc.garch12_d.spec, data = tst, fit.control=list(scale=TRUE))
  
  # Obtain conditional Correlation..
  r1=rcor(dcc.fit, type="R")
  r1.z=zoo(r1[1,2,], order.by=time(tst))
  datetemp<-index(r1.z)
  datatemp<-as.data.frame(r1.z)
  colnames(datatemp)<-"datatemp"
  datatemp<-cbind(datetemp, datatemp)
  return(datatemp)
}