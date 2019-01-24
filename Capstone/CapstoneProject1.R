########## Personality and IQ effect on Income
library(mice)
library(VIM)
library(boot)
library(purrr)
library(readxl)
library(leaps)
library(ggplot2)
library(caret)
library(maps)
library(MASS)
library(tree)
library(ISLR)
library(lava)
library(caret)

###rememberto account for missing IQs and Incomes
setwd("C:/Users/Warren M/Desktop/DataScience/projectsnother/psych")
ta=read.csv("Males_data.csv")

a=t(ta)

b=as.data.frame(a)
b2=as.data.frame(t(b)) #creates dataframe b2 with labels on left
nrow(b2)

#create vector with 0 for thow out and 1 for keep
NAs=as.data.frame(map_dbl(b,~sum(is.na(.))/nrow(b)))
natf=as.data.frame(matrix(data=NA, nrow(NAs),1))
for (i in 1:nrow(NAs)) {
  if (NAs[i,]<=.36) {
    natf[i,]=1
  } else { natf[i,]=0}}
NAs[,2]=b2[,1]

##Prepare to erase rows w/ too many NAs 
z=rep(NA, nrow(a))
nrow(b2)
headers=b2[,1:3]

income=as.data.frame(rep(1,nrow(b2),3))

for (i in 1:ncol(b2)) {
  b2[,i]=as.numeric(as.character(b2[,i]))
}

b1=b2

for (i in 1:nrow(b2)) {
  for (j in 1:ncol(b2)) {
    if (is.na(b2[i,j])) {
      b2[i,j]=(as.numeric(b2[i,5]))
    }}}

for (i in 1:nrow(b2)) {
  for (j in 7:ncol(b2)) {
    b2[i,j]=(b2[i,4]*(b2[i,j]-b2[i,5])/b2[i,6])
  }
}


b2[,1:3]=headers #reinsert row labels#

#set rows with >1/3 NA to all NA
for (i in 1:ncol(a)) {
  if (natf[i,]==0) {
    b2[i,]=z
  }
}
#remove those rows
b3=na.omit(b2)

traits=c("Agreeableness", "Confidence", "Conscientiousness", "Openness", "Extroversion",
         "Happiness", "Health", "Income", 
         "IQ", "Language", "Leadership", "Morality", "Neurotic", "Optimism", 
         "Perseverance", "Physicality", "Popularity", "Religiousness", 
         "STEM")

finaldata=matrix(data=NA, nrow=length(traits), ncol=ncol(b3))
finaldata[,1]=traits

for (i in 1:length(traits)) {
  current_class=subset(b3, Class==traits[i])
  for (j in 2:ncol(b3)) {
    finaldata[i,j]=mean(current_class[,j])
  }
}
final_data=na.omit(as.data.frame(t(finaldata)))

fnames=c(t(final_data[1,]))
for (i in 1:ncol(final_data)) {
  final_data[,i]=as.numeric(as.character(final_data[,i]))
}
colnames(final_data)=fnames
final_data=final_data[-c(1:4),]

Agreeableness=lm(Agreeableness~., data=final_data)
Confidence=lm(Confidence~., data=final_data)
Conscientiousness=lm(Conscientiousness~., data=final_data)
Openness=lm(Openness~., data=final_data)
Extroversion=lm(Extroversion~., data=final_data)
Happiness=lm(Happiness~., data=final_data)
Health=lm(Health~., data=final_data)
Income=lm(Income~., data=final_data)
IQ=lm(IQ~., data=final_data)
Language=lm(Language~., data=final_data)
Leadership=lm(Leadership~., data=final_data)
Morality=lm(Morality~., data=final_data)
Neurotic=lm(Neurotic~., data=final_data)
Optimism=lm(Optimism~., data=final_data)
Perseverance=lm(Perseverance~., data=final_data)
Physicality=lm(Physicality~., data=final_data)
Popularity=lm(Popularity~., data=final_data)
Religiousness=lm(Religiousness~., data=final_data)
STEM=lm(STEM~., data=final_data)

summary(STEM)

finalData=as.data.frame(final_data)
glm.fit=glm(IQ~.,data=final_data)
cv.error=cv.glm(final_data,glm.fit,K=10)$delta[1]
summary(cv.error)

Money=lm(Income~IQ+Agreeableness+Openness+Conscientiousness+Neurotic+Extroversion, data=final_data)
Happy=lm(Happiness~IQ+Agreeableness+Openness+Conscientiousness+Neurotic+Extroversion, data=final_data)

#Examine results

summary(Agreeableness)
summary(Neurotic)
summary(Openness)
summary(Conscientiousness)
summary(Extroversion)
summary(STEM)
summary(Income)
summary(Language)
summary(Optimism)

#Graphing Leadership
par(mfrow=c(3,2))

plot(final_data$Leadership, final_data$Conscientiousness)
abline(lm(Leadership ~ Conscientiousness, data=final_data), lwd=2, col="blue")

plot(final_data$Leadership, final_data$Openness)
abline(lm(Leadership ~ Openness, data=final_data),  lwd=2,col="blue")

plot(final_data$Leadership, final_data$Morality, xlim=c(-1.5,1.5), ylim=(c(-1.4,1.5)))
abline(lm(Leadership ~ Morality, data=final_data), lwd=2, col="blue")

plot(final_data$Leadership, final_data$Popularity)
abline(lm(Leadership ~ Popularity, data=final_data), lwd=2, col="blue")

plot(final_data$Leadership, final_data$Perseverance)
abline(lm(Leadership ~ Perseverance, data=final_data), lwd=2, col="blue")