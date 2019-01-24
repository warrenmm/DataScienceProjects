setwd("C:/Users/Warren M/Desktop/DataScience/projectsnother/psych")

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
library(tictoc)
library(matrixStats)

a=read.csv("m2.csv")
b=a
a=b
a=a[,-c(1,2,4)]

for (i in 2:ncol(a)) {
  a[1,i]=max(a[51:52,i],na.rm=T)
}

for (i in 2:ncol(a)) {
  if(a[1,i]==0){
    a[1,i]=NA
  }}
tic()


for (i in 2:nrow(a)) {
  for (j in 2:ncol(a)) {
    if (is.na(a[i,j])) {
      a[i,j]=9
    }}}
toc()
tic()
for (i in 2:nrow(a)) {
  for (j in 2:ncol(a)) {
    if (a[i,j]==9){
      a[i,j]=99
    }}} 
toc()
tic()
    for (i in 2:nrow(a)) {
      for (j in 2:ncol(a)) {    
      if (a[i,j]==99){
        a[i,j]=999
      }}}
toc()
tic()

        for (i in 2:nrow(a)) {
          for (j in 2:ncol(a)) {
        if (a[i,j]==999){
          a[i,j]=9999
        }}}
toc()
tic()

          for (i in 2:nrow(a)) {
            for (j in 2:ncol(a)) {
          if (a[i,j]==9999){
            a[i,j]=NA
          }}}
toc()
a1=a
for (i in 2:nrow(a)) {
  a[2,i]=max(as.numeric(a[3:6,i]),na.rm=TRUE)
  if (a[2,i]==-Inf) 
    a[2,i=NA]
}

fix(a)
a=a[,-1]


write.csv(a,"a3.csv")
a=read.csv("a3.csv")

b=as.data.frame(t(a))
fix(b)

colnames(b)=a[,1]
b=b[-1,]
for (i in 1:ncol(b)) {
  b[,i]=as.numeric(as.character(b[,i]))
}
b=b[-1,]
fix(b)

b2=b

tic()
for (j in 1:ncol(b2)) {
  meanimpute=mean(b2[,j],na.rm=T)
  for (i in 1:nrow(b2)) {
    if (is.na(b2[i,j])) {
      b2[i,j]=meanimpute 
    }
  }
}
toc()
write.csv(b2,"B2.csv")
bcorrelation=b2
bcorrelation=read.csv("B2.csv")
bcorrelation=bcorrelation[1:856,]

str(bcorrelation)
fix(bcorrelation)
bcor=cor(bcorrelation[,])
write.csv(bcor,"bcor.csv")
fix(bcor)


#Look in bcor for good indicators:

iqpredictors=c("IQ","MOOD RANGE BETWEEN HIGH AND LOW RATINGS",
"DESIRE TO KNOW/TEACHER RATING OF", "DESIRE TO KNOW/CURIOSITY",
"HEARING", "STILL PLAY SOLITARY GAMES/PUZZLES?",
"# CHILDREN SPOKEN TO NOT AT ALL", "  TRUTHFULNESS",
"STILL PLAY SOLITARY GAMES/PUZZLES?", "DEGREES/SCHOOLING: FATHER",
"READING FICTION", "LITERATURE/AMOUNT OF INTEREST IN", 
"CHILD'S CHEERFULNESS AND OPTIMISM", 
"CONSCIENTIOUSNESS/TEACHER RATING OF", 
"ORIGINALITY", "HEALTH RATING", "VITAL STATUS AS OF JUNE, 1983", 
"JOY IN LIVING/SUCCESS", "WATCH SPORTS ON TV", 
"SOCIAL ABILITY/CHILDHOOD/ADOLESCENCE", 
"PERSONALITY TRAITS/SELF-CONFIDENCE", 
"POLITICAL AND ECONOMIC VIEWPOINT", 
"FRIENDSHIPS/LIFETIME SATISFACTION", 
"OUTDOOR SPORTS/AMOUNT OF INTEREST IN")

incomepredictors=c("peakIncome","TOO INTERESTED IN SOCIAL AFFAIRS/SPOUSE",
"GOSSIPS INDESCREETLY/SPOUSE", "RECREATION/% OF TIME 1966-72", 
"OUTDOOR SPORTS/AMOUNT OF INTEREST IN", "WEIGHT OF SUBJECT IN POUNDS",
"LIBRARIANS/FEELINGS TOWARD", "LIKES ZOOLOGY?",
"WRITING PERSONAL LETTERS/FEELINGS TOWARD", "WORK PERSISTENCE INDEX",
"POLITICAL AND ECONOMIC VIEWPOINT", "FEELING ABOUT PRESENT VOCATION", 
"COOKING SKILL", "TEND TO BE DOMINANT WITH OPP SEX?",
"CHANGE IN AMOUNT OF INTEREST IN RELIGION",
"POLITICAL AND ECONOMIC VIEWPOINT", "COOKING SKILL", 
"FINANCIAL GAIN/AMBITION SINCE AGE 40", "PRESENT FEELINGS ABOUT JOB",
"CHILD'S AMOUNT OF PHYSICAL ENERGY", "INCOME SATISFACTION", 
"COMPETE IN SPORTS", "OBSERVING OUTDOOR SPORTS?")

colnames(b)[2]="peakIncome"

b5=as.data.frame(t(b))
b5=b
b6=b5

rownames(b6)=make.names(rownames(b6),unique=TRUE)
iqpredictors=make.names(iqpredictors,unique=TRUE)
incomepredictors=make.names(incomepredictors,unique=TRUE)

iqset=subset(b6, rownames(b6) %in% iqpredictors)
incomeset=subset(b6, rownames(b6) %in% incomepredictors)

length(incomepredictors)
length(iqpredictors)

iqset=as.data.frame(t(iqset))
incomeset=as.data.frame(t(incomeset))

for (i in 1:nrow(incomeset)) {
  if (incomeset[i,1]==-Inf) {
    incomeset[i,1]=NA
  }
}

#Fill in NA's

iqset=kNN(iqset)
incomeset=kNN(incomeset)

iqset=iqset[,1:23]
incomeset=incomeset[,1:21]

iqset=iqset[1:856,]
incomeset=incomeset[1:856,]

IQlm=lm(IQ~.,iqset)
summary(IQlm)
fix(iqset)
Incomelm=lm(peakIncome~.,incomeset)
summary(Incomelm)
fix(IQlm)
fix(b6)
