rm(list=ls())
setwd("C:/Users/Warren M/Desktop/Data Science/Week2/Project2/Project2b")
#install.packages("dplyr") 
LoadLibraries=function(){
  library(ISLR)
  library(dplyr)
  library(MASS)
  library(readxl)
  library(tseries)
  library(car)
  print("The libraries have been loaded.")
}
LoadLibraries()

data1=read.csv(paste0("ACS_09_5YR_DP02_with_ann.csv"))
data2=as.data.frame(data1[c(2,3,232,238,242,246,250,254,258,262)],stringsAsFactors=FALSE)
data2[,11]=NA
data2[1,11]="CHCI09"
for (id in 2:nrow(data2)){
  data2[id,11]=(50*as.numeric(as.character(data2[id,4]))+100*as.numeric(as.character(data2[id,5]))+120*as.numeric(as.character(data2[id,6]))+130*as.numeric(as.character(data2[id,7]))+140*as.numeric(as.character(data2[id,8]))+190*as.numeric(as.character(data2[id,9]))+230*as.numeric(as.character(data2[id,10])))/100
}
chci=data.frame(data2[,c(-3,-4,-5,-6,-7,-8,-9,-10)],stringsAsFactors=FALSE)
chci[,c(4:22)]=NA
chci[,3]=data2[,11]
chci[,11]=data2[,3]
for (year in 10:16){
  data1=read.csv(paste0("ACS_",year,"_5YR_DP02_with_ann.csv"))
  data2=as.data.frame(data1[c(2,3,232,238,242,246,250,254,258,262)])
  data2[,11]=NA
  data2[1,11]=paste0("CHCI",year)
  if (as.numeric(as.character(data2[2918,1]))==51520){
    bedford <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    data2 <- rbind(data2[1:2917, ], bedford, data2[2918:nrow(data2), ])
  }
  for (id in 2:nrow(data2)){
    data2[id,11]=(50*as.numeric(as.character(data2[id,4]))+100*as.numeric(as.character(data2[id,5]))+120*as.numeric(as.character(data2[id,6]))+130*as.numeric(as.character(data2[id,7]))+140*as.numeric(as.character(data2[id,8]))+190*as.numeric(as.character(data2[id,9]))+230*as.numeric(as.character(data2[id,10])))/100
    indexRecipientNumber=year-6
    chci[,indexRecipientNumber]=data2[,11]
    populationRecipientNumber=year+2
    chci[,populationRecipientNumber]=data2[,3]
  }
}
for (rates in 2:nrow(chci)){
  chci[rates,19]=as.numeric(chci[rates,10])-as.numeric(chci[rates,3])
  chci[rates,20]=(as.numeric(chci[rates,10])-as.numeric(chci[rates,3]))/(as.numeric(chci[rates,3]))
  chci[rates,21]=as.numeric(as.character(chci[rates,18]))-as.numeric(as.character(chci[rates,11]))
  chci[rates,22]=(as.numeric(as.character(chci[rates,18]))-as.numeric(as.character(chci[rates,11])))/(as.numeric(as.character(chci[rates,11])))
}
colnames(chci)=c("id","county","chci09","chci10","chci11","chci12","chci13","chci14","chci15","chci16","pop09","pop10","pop11","pop12","pop13","pop14","pop15","pop16","chcig","chcigr","popg","popgr")
chci=chci[-1,]
chci2=chci
filler1=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
filler2=as.data.frame(filler1)
filler=as.data.frame(t(filler2))
colnames(filler)=c("id","county","chci09","chci10","chci11","chci12","chci13","chci14","chci15","chci16","pop09","pop10","pop11","pop12","pop13","pop14","pop15","pop16","chcig","chcigr","popg","popgr")
desc.chci16=top_n(arrange(chci, desc(chci16)), 20)
desc.chcig=top_n(arrange(chci, desc(chcig)), 20)
desc.pop16=top_n(arrange(chci, desc(pop16)), 20)
desc.popg=top_n(arrange(chci, desc(popg)), 20)
desc.popgr=top_n(arrange(chci, desc(popgr)), 20)
asc.chci16=top_n(arrange(chci, desc(chci16)), -20)
asc.chcig=top_n(arrange(chci, desc(chcig)), -20)
asc.popg=top_n(arrange(chci, desc(popg)), -20)
asc.popgr=top_n(arrange(chci, desc(popgr)), -20)
chci.top20=rbind(desc.chci16,filler,desc.chcig, filler,desc.pop16,filler,desc.popg, filler,desc.popgr, filler,asc.chci16,filler,asc.chcig,filler,asc.popg,filler,asc.popgr)
#setwd("C:/Users/Warren M/Desktop/Data Science/Week2/Project2")
#write.xlsx(chci, "chci.WarrenMardoum.xlsx")
#write.xlsx(chci.top20, "chci.top20.WarrenMardoum.xlsx")
