library("zoo")
library("tseries")
library("ggplot2")
library("EMD")
library("xlsx")
library("e1071")

setwd("C:/Users/angela.zhou/Desktop/angela_dissertation/market_state")
cl1_price<- read.xlsx("./data/hk/cl1_10year.xlsx",1)
gold_price<- read.xlsx("./data/hk/gold_10year.xlsx",1)
spx_price<- read.xlsx("./data/hk/spx_10year.xlsx",1)
usd_price<- read.xlsx("./data/hk/usd_10year.xlsx",1)


#remove noise by applying emd
cl1_emd <- emd(as.numeric(as.vector(cl1_price[,2])),cl1_price[,1],boundary="wave")
cl1_emd_res <- as.numeric(as.vector(cl1_price[,2]))- cl1_emd$imf[,1] - cl1_emd$imf[,2] - cl1_emd$imf[,3] 

#remove noise by applying emd
gold_emd <- emd(as.numeric(as.vector(gold_price[,2])),gold_price[,1],boundary="wave")
gold_emd_res <- as.numeric(as.vector(gold_price[,2]))- gold_emd$imf[,1] - gold_emd$imf[,2] - gold_emd$imf[,3] 

#remove noise by applying emd
spx_emd <- emd(as.numeric(as.vector(spx_price[,2])),spx_price[,1],boundary="wave")
spx_emd_res <- as.numeric(as.vector(spx_price[,2]))- spx_emd$imf[,1] - spx_emd$imf[,2] - spx_emd$imf[,3] 

#remove noise by applying emd
usd_emd <- emd(as.numeric(as.vector(usd_price[,2])),usd_price[,1],boundary="wave")
usd_emd_res <- as.numeric(as.vector(usd_price[,2]))- usd_emd$imf[,1] - usd_emd$imf[,2] - usd_emd$imf[,3] 


label_inteval<-seq(from=1,to=length(cl1_price[,1]), by=18)
label_date<-cl1_price[,1][label_inteval]
plot(cl1_emd_res,col="red",xaxt='n',type ="l",ann=FALSE)
axis(1, at=label_inteval, labels=label_date,las=2)

getNdaysSignalBeofore <- function(factorData,factorDataRes,date,nDays){

  if(length(which(factorData[,1]==date))!=0){
    endDate <- which(factorData[,1]==date)
  }else if(length(which((factorData[,1]-1)==date))!=0){
    endDate <- which((factorData[,1]-1)==date)
  }else if(length(which((factorData[,1]-2)==date))!=0){
    endDate <- which((factorData[,1]-2)==date)
  }else if(length(which((factorData[,1]-3)==date))!=0){
    endDate <- which((factorData[,1]-3)==date)
  }else if(length(which((factorData[,1]-4)==date))!=0){
    endDate <- which((factorData[,1]-4)==date)
  }
  startDate <- endDate - nDays
  changeRate<- round((factorDataRes[endDate]- factorDataRes[startDate])/factorDataRes[startDate], digits=2)
  if(changeRate>=0.02){
    return('U')
  }else if(changeRate<= -0.02){
    return('D')
  }else
    return('S')
}

cl1_signals<-{}
gold_signals<-{}
spx_signals<-{}
usd_signals<-{}
for(i in 1: (nrow(hsi_seg_signal)-1)){
  cl1_signals<- rbind(cl1_signals, getNdaysSignalBeofore(cl1_price, cl1_emd_res,hsi_seg_signal[i+1,1],20))
  gold_signals<- rbind(gold_signals, getNdaysSignalBeofore(gold_price, gold_emd_res,hsi_seg_signal[i+1,1],20))
  spx_signals<- rbind(spx_signals, getNdaysSignalBeofore(spx_price, spx_emd_res,hsi_seg_signal[i+1,1],20))
  usd_signals<- rbind(usd_signals, getNdaysSignalBeofore(usd_price, usd_emd_res,hsi_seg_signal[i+1,1],20))
}

hsi_4factor<-data.frame(hsi_seg_signal[-1,1],hsi_seg_signal[-1,2],cl1_signals,gold_signals,spx_signals,usd_signals)
colnames(hsi_4factor)<- c("date", "hsi","cl1","gold","spx","usd")
write.csv(hsi_4factor, "hsi_4factor.csv")

hsi_4factor_nolag<-data.frame(hsi_seg_signal[-length(hsi_seg_signal),1],hsi_seg_signal[-length(hsi_seg_signal),2],cl1_signals,gold_signals,spx_signals,usd_signals)
colnames(hsi_4factor_nolag)<- c("date", "hsi","cl1","gold","spx","usd")
write.csv(hsi_4factor_nolag, "hsi_4factor_nolag.csv")
