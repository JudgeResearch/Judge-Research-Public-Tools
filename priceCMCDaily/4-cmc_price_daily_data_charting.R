# =======================================================
# chart dcc sinc jan 1 2016
date_r<-as.Date(data_r[,1], format="%m/%d/%Y")
date_p<-as.Date(data_p[,1], format="%m/%d/%Y")
date_vec<-date_r
data_r[,-1] <- lapply(data_r[,-1], function(x) as.numeric(x))
data_p[,-1] <- lapply(data_p[,-1], function(x) as.numeric(x))
data_dcc[,-1]<- lapply(data_dcc[,-1],function(x) as.numeric(x))
coinSummary_2 <- gsub('_USDT','',coinSummary$coin)
coinSummary_2 <- gsub('_USD','',coinSummary_2)

date_vec<-date_p
time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "quarter"), format="%Y")
time_label<-character()
for (i in (1:length(time_label_y))){
  if (i %% 4 == 1){
    time_label[i]<-time_label_y[i]
  } else if (i %% 4 == 2){
    time_label[i]<-"Q2"
  } else if (i %% 4 == 3) {
    time_label[i]<-"Q3"
  } else {
    time_label[i]<-"Q4"
  }
}
timeFreq<-"quarter"

# yaxis label
y_label<-seq(-1, 1, by=0.2)

# plot DCC data series-daily
i<-1; j<-2

for (i in (1:length(coinSummary$coin))){
  for (j in (1:length(coinSummary$coin))){
    data1_p<-eval(parse(text=paste0("data_p$",coinSummary[i,1],"_p")))
    data2_p<-eval(parse(text=paste0("data_p$",coinSummary[j,1],"_p")))
    data1_p<-data1_p*(1/data1_p[max(which(data1_p>0)[1],424)]) # use 3/1/2017 as baseline if it is higher
    data2_p<-data2_p*(1/data2_p[max(which(data2_p>0)[1],424)])
    
    vec_length <-length(data1_p)
    # create xts object for plot
    if (i==j){
      dcc_vec <- rep(1,vec_length)
    } else if(j>i) {
      dcc_vec <- eval(parse(text = paste0("data_dcc$", colnames(data_r)[i+1],"_", colnames(data_r)[j+1])))
    } else {
      dcc_vec <- eval(parse(text = paste0("data_dcc$", colnames(data_r)[j+1],"_", colnames(data_r)[i+1])))
    }
    
    #
    postscript(file.path("./chart",paste0(cmc_coin$coin[i],"_usd_",cmc_coin$coin[j],"_usd_dcc_jan2016.eps")), fonts="serif",
               width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")
    
    # plot fucntion
    plotDccdata(date_vec, time_label, y_label, timeFreq, dcc_vec, data1_p, data2_p, coinSummary_2, i, j)
    
    # add sub plot of volume
    dev.off()
    message(i," , ", j)
  }
}

# =======================================================
# chart dcc since mar 1 2018
date_vec<-date_p[startIndex2:endIndex]
time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b")
time_label<-time_label_y
timeFreq<-"month"
# # create xaxis label
# # time_label_m<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "month"), format="%b")
# time_label_y<-strftime(seq(date_vec[1], date_vec[length(date_vec)], by = "quarter"), format="%Y")
# time_label<-character()
# for (i in (1:length(time_label_y))){
#   if (i %% 4 == 1){
#     time_label[i]<-time_label_y[i]
#   } else if (i %% 4 == 2){
#     time_label[i]<-"Q2"
#   } else if (i %% 4 == 3) {
#     time_label[i]<-"Q3"
#   } else {
#     time_label[i]<-"Q4"
#   }
# }

# yaxis label
y_label<-seq(-1, 1, by=0.2)

# plot DCC data series-daily
i<-1; j<-9

for (i in (1:length(coinSummary$coin))){
  for (j in (1:length(coinSummary$coin))){
    data1_p<-eval(parse(text=paste0("data_p$",coinSummary[i,1],"_p")))
    data2_p<-eval(parse(text=paste0("data_p$",coinSummary[j,1],"_p")))
    data1_p<-data1_p*(1/data1_p[max(which(data1_p>0)[1],424)]) # use 3/1/2017 as baseline if it is higher
    data2_p<-data2_p*(1/data2_p[max(which(data2_p>0)[1],424)])
    data1_p<-data1_p[startIndex2:endIndex]
    data2_p<-data2_p[startIndex2:endIndex]
    
    vec_length <-length(data1_p)
    # create xts object for plot
    if (i==j){
      dcc_vec <- rep(1,vec_length)
    } else if(j>i) {
      dcc_vec <- eval(parse(text = paste0("data_dcc$", colnames(data_r)[i+1],"_", colnames(data_r)[j+1])))[startIndex2:endIndex]
    } else {
      dcc_vec <- eval(parse(text = paste0("data_dcc$", colnames(data_r)[j+1],"_", colnames(data_r)[i+1])))[startIndex2:endIndex]
    }
    #
    postscript(file.path("./chart",paste0(cmc_coin$coin[i],"_usd_",cmc_coin$coin[j],"_usd_dcc_mar2018.eps")), fonts="serif",
               width = 15, height =7.5, horizontal = TRUE, family= "serif", paper ="special")
    
    # plot fucntion
    plotDccdata(date_vec, time_label, y_label, timeFreq, dcc_vec, data1_p, data2_p, coinSummary_2, i, j)
    
    # add sub plot of volume
    dev.off()
    message(i," , ", j)
  }
}

message("DCC since March 2018 is done~!")