setwd("C:/Users/Warren M/Desktop/DataScience/projectsnother/psych")
library(purrr)
library(readxl)
library(leaps)
library(caret)
library(maps)


female_data <- read.table("00882Terman-LifeCycle-Women-Data.tab", sep="\t", header=FALSE)
male_data <- read.table("00882Terman-LifeCycle-Male-Data.tab", sep="\t", header=FALSE)
longitudinal_data <- read.table("00882Terman-LifeCycle-Longitudinal-Data.tab", sep="\t", header=FALSE)

flabels=read_xlsx("female_labels.xlsx")
mlabels=read_xlsx("male_labels.xlsx")
llabels=read_xlsx("longitudinal_labels.xlsx")

llabels=na.omit(llabels)
mlabels=na.omit(mlabels) 
flabels=na.omit(flabels)

tfem=t(female_data)
tmale=t(male_data)
tlong=t(longitudinal_data)


colnames(tlong)=tlong[1,]
colnames(tmale)=tmale[1,]
colnames(tfem)=tfem[1,]

tl=merge(llabels,tlong, by="IDN")
tm=merge(mlabels,tmale, by="ID")
tf=merge(flabels,tfem, by="ID")

f1=as.data.frame(t(tf))
l1=as.data.frame(t(tl))
m1=t(tm)

renamevec=c(colnames(tm))
renamevec[2]="cn"
colnames(tm)=renamevec
try=tm$cn

for (i in 1:length(try)) {
  czech=try[1:i]
  for (j in 1:(length(czech))){
    if (try[i]==czech[j]){
      try[i]=paste(as.character(try[i]),as.character(i),sep=" ")
  }}}
#can be reduced; renames everything to include its var numberto avoid duplicates
  

try[2]="iq"

colnames(m1)=try

m1=m1[-1,];m1=m1[-1,];

for (i in 1:nrow(m1)) {
  m1[i,2]=max(c(m1[i,51],m1[i,52]))
}

m2=as.data.frame(m1)

for (i in 1:ncol(m2)) {
  m2[,i]=as.numeric(as.character(m2[,i]))
}

m2=as.data.frame(t(na.omit(t(m2))))
for (i in 1:nrow(m2)) {
  for (j in 1:ncol(m2)) {
    if (m2[i,j]==99) {
      m2[i,j]=NA
    } else {
      if (m2[i,j]==999) {
        m2[i,j]=NA 
      } else { 
        if (m2[i,j]==9){
          m2[i,j]=NA
}}}}}

nas=as.data.frame(map_dbl(m2,~sum(is.na(.))/nrow(m2)))
m3=as.data.frame(t(na.omit(t(m2))))

str(m3)

check2=lm(iq~.,data=m2)
summary(investigate)

write.csv(tm, "m2.csv")
