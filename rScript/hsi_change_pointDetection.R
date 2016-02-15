library("zoo")
library("tseries")
library("ggplot2")
library("EMD")
library("xlsx")
library("e1071")

setwd("C:/Users/angela.zhou/Desktop/angela_dissertation/market_state")
hsi_price<- read.xlsx("./data/hk/hsi_index_10year.xlsx",1)

#remove noise by applying emd
hsi_emd <- emd(as.numeric(as.vector(hsi_price[,2])),hsi_price[,1],boundary="wave")
hsi_emd_res <- as.numeric(as.vector(hsi_price[,2]))- hsi_emd$imf[,1] - hsi_emd$imf[,2] - hsi_emd$imf[,3] 


#plot hsi graph
label_inteval<-seq(from=1,to=length(hsi_price[,1]), by=18)
label_date<-hsi_price[,1][label_inteval]
plot(hsi_emd_res,col="red",xaxt='n',type ="l",ann=FALSE)
axis(1, at=label_inteval, labels=label_date,las=2)

#get the first different of emd to detect the trend change
firstD<- diff(hsi_emd_res) 


#get the segmentation point
result<- {}
#handle the first point
result<- rbind(result,1)
for(i in 1:(length(firstD)-1)){
  if(firstD[i]*firstD[i+1]<0){
    result<- rbind(result,i+1)
  }
}
# handle the last point
if(result[length(result)]!=nrow(hsi_price)){
  result<- rbind(result, nrow(hsi_price))
}

#get the segment duration
segDuration<- diff(result)

# function to split and merge segDuration
splitSequence <- function(segDuration){
  #split first
  for(i in 1: length(segDuration)){
    if(segDuration[i]>40){
      half<-segDuration[i]/2
      segDuration[i]<-ceiling(half)
      segDuration <- insertInto(segDuration,i,floor(half))
    }
  }
  return(segDuration)
}
mergeSequence <- function(segDuration){
  #split first
  for(i in 1: (length(segDuration))){
    if(length(segDuration)>=i){
      if(segDuration[i]<15){
        if(i==1){
          segDuration[i+1]<- segDuration[i]+segDuration[i+1]
          segDuration<-deleteBy(segDuration,i)
        }else if(i==length(segDuration)){
          segDuration[i-1]<- segDuration[i]+segDuration[i-1]
          segDuration<-deleteBy(segDuration,i)
        }else if(i>1&&segDuration[i+1]>segDuration[i-1]){
          segDuration[i-1]<- segDuration[i]+segDuration[i-1]
          segDuration<-deleteBy(segDuration,i)
        }else{
          segDuration[i+1]<- segDuration[i]+segDuration[i+1]
          segDuration<-deleteBy(segDuration,i)
        }
      }
    }
  }
  return(segDuration)
}
insertInto <- function(DF, position,data){
  
  d_result <- append(DF[c(1:(position))],data)
  if(position+1<=length(DF))
    d_result<-append(d_result,DF[c((position+1):length(DF))])
  return(d_result)
}
deleteBy <- function(DF, position){
  if((position-1)>=1&&(position+1)<=length(DF)){
    d_result<- append(DF[c(1:(position-1))],DF[c((position+1):length(DF))])
  }else if((position-1)==0)
    d_result<-DF[c((position+1):length(DF))]
  else if(position==length(DF)) 
    d_result<-DF[c(1:(position-1))]
  return(d_result)
}

rMergeTest<-splitSequence(segDuration)
rMergeTest<-mergeSequence(rMergeTest)
rMergeTest<-splitSequence(rMergeTest)
rMergeTest<-mergeSequence(rMergeTest)

currentIndex<-1
changeRates<-{}
comIndex <-{}
comIndex <- rbind(comIndex,currentIndex )
signals <- {}
for(i in 1:length(rMergeTest)){
  newIndex<- currentIndex + rMergeTest[i]
  changeRate <- round((hsi_emd_res[newIndex]-hsi_emd_res[currentIndex])/hsi_emd_res[currentIndex],digits=2) 
  if(changeRate>=0.02){
    signals<- append(signals, 'U')
  }else if(changeRate <= -0.02){
    signals<- append(signals,'D')
  }else {
    signals<- append(signals,'S')
  }
  #if(changeRate>=0){
   # signals<- append(signals, 'U')
  #}else{
   # signals<- append(signals, 'S')
  #}
  changeRates <- rbind(changeRates,changeRate)
  currentIndex<- newIndex
  comIndex <- rbind(comIndex,currentIndex )
}
segDates<- hsi_price[comIndex,1]
hsi_seg_signal<- data.frame(segDates[-length(segDates)],signals)
write.csv(hsi_seg_signal, "seg_signals.csv")
