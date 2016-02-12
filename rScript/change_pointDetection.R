library("zoo")
library("tseries")
library("ggplot2")
library("EMD")
library("xlsx")
library("e1071")
library("TSdist")
library("TSclust")

setwd("C:/Users/angela.zhou/Desktop/angela_dissertation/market_state")
hsi_price<- read.xlsx("./data/hk/hsi_index_10year.xlsx",1)
hsi_emd <- emd(as.numeric(as.vector(hsi_price[,2])),hsi_price[,1],boundary="wave")
hsi_emd_res <- as.numeric(as.vector(hsi_price[,2]))- hsi_emd$imf[,1] - hsi_emd$imf[,2] - hsi_emd$imf[,3] 
label_inteval<-seq(from=1,to=length(hsi_price[,1]), by=18)
label_date<-hsi_price[,1][label_inteval]
plot(hsi_emd_res,col="red",xaxt='n',type ="l",ann=FALSE)
axis(1, at=label_inteval, labels=label_date,las=2)

#change point and segmentation 

firstD<- diff(hsi_emd_res) 

result<- {}
result<- rbind(result,hsi_price[1,1])

for(i in 1:(length(firstD)-1)){
    if(firstD[i]*firstD[i+1]<0){
        result<- rbind(result, hsi_price[i+1,1])
    }
}
segDuration<- diff(result)

#merge and split Sequence
splitSequence <- function(segDuration){
      #split first
      for(i in 1: length(segDuration)){
        if(segDuration[i]>60){
          half<-segDuration[i]/2
          segDuration[i]<-ceiling(half)
          segDuration <- insertInto(segDuration,i+1,floor(half))
        }
      }
    return(segDuration)
}
mergeSequence <- function(segDuration){
  #split first
  for(i in 1: (length(segDuration))){
    if(length(segDuration)>i){
      if(segDuration[i]<20){
         
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
    if((position-1)>=1){
      d_result <- append(DF[c(1:(position-1))],data)
    }
    if((position+1)<=length(DF)){
      d_result<-append(d_result,DF[c((position+1):length(DF))])
    }
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

rTest<-splitSequence(segDuration)
rTest<-splitSequence(rTest)
rMergeTest<-mergeSequence(rTest)
rMergeTest<-splitSequence(rMergeTest) 

which(hsi_price[,1]==(hsi_price[1,1]+43))

cl1_price<- read.xlsx("./data/hk/cl1_10year.xlsx",1)
cl1_emd <- emd(as.numeric(as.vector(cl1_price[,2])),cl1_price[,1],boundary="wave")
cl1_emd_res <- as.numeric(as.vector(cl1_price[,2]))- cl1_emd$imf[,1] - cl1_emd$imf[,2] - cl1_emd$imf[,3] 
label_inteval<-seq(from=1,to=length(cl1_price[,1]), by=18)
label_date<-cl1_price[,1][label_inteval]
plot(cl1_emd_res,col="red",xaxt='n',type ="l",ann=FALSE)
axis(1, at=label_inteval, labels=label_date,las=2)


label_inteval<-seq(from=1,to=1260, by=18)
label_date<-cl1_price[,1][label_inteval]
plot(cl1_emd_res[c(1:1260)],col="red",xaxt='n',type ="l",ann=FALSE)
axis(1, at=label_inteval, labels=label_date,las=2)

label_date<-cl1_price[,1][label_inteval]
plot(cl1_emd_res[c(1261:2520)],col="red",xaxt='n',type ="l",ann=FALSE)
axis(1, at=label_inteval, labels=label_date,las=2)

#change point and segmentation 

cl1_firstD<- diff(cl1_emd_res) 

result<- {}

for(i in 1:(length(firstD)-1)){
  if(firstD[i]*firstD[i+1]<0){
    result<- rbind(result, cl1_price[i+1,1])
  }
}
segDuration<- diff(result)

getPastOneMonthTrend <- function(data, date){
  
  
}
currentIndex<-1
for(i in 1:length(rMergeTest)){
    print(i)
    print(currentIndex)
    print(hsi_price[currentIndex,1]+(rMergeTest[i]))
    if(length(which(hsi_price[,1]==(hsi_price[currentIndex,1]+(rMergeTest[i]))))!=0){
      newIndex<-which(hsi_price[,1]==(hsi_price[currentIndex,1]+(rMergeTest[i])))
    }else if(length(which(hsi_price[,1]==(hsi_price[currentIndex,1]+(rMergeTest[i]+1))))!=0) {
      newIndex<-which(hsi_price[,1]==(hsi_price[currentIndex,1]+(rMergeTest[i]+1)))
    }else if(length(which(hsi_price[,1]==(hsi_price[currentIndex,1]+(rMergeTest[i]+2))))!=0){
      newIndex<-which(hsi_price[,1]==(hsi_price[currentIndex,1]+(rMergeTest[i]+2)))
    }else {
      newIndex<-which(hsi_price[,1]==(hsi_price[currentIndex,1]+(rMergeTest[i]+5)))
    }
    currentIndex<- newIndex
}
