##################################################################################################
# P06
# Script Answer to Project 6 UCLA Extension Data Science Intensive
# 11/9/2018 by William Yu
##################################################################################################
setwd("C:/Users/Warren M/Desktop/DataScience/datasets")
#install.packages("DataCombine")
library(car)
#install.packages("imputeTS")
#install.packages("mxnet")
#library(mxnet)
library(quantmod) 
library(tseries)
library(forecast)
library(readxl) 
library(fpp2)
library(dynlm) 
library(vars)
library(tseries)                  # You need to call on library every time you restart the R script
library(e1071)
library(DataCombine)
library(VIM)
library(zoo)
library(readxl) 
library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(stringr)
library(DT)
library(corrplot)
library(lubridate)
library(neuralnet) 
library(leaps)
library(caret)
library(boot)
library(tree)
library(randomForest)
library(lava)
wdi <- data.frame(read_excel("W03b_wdi.xlsx"))     # World Development Indicators


remove1=c("X1960","X1961","X1962","X1963","X1964","X1965","X1966","X1967","X1968","X1969",
          "X1970","X1971","X1972","X1973","X1974","X1975","X1976","X1977","X1978","X1979","X1980")

# Remove years from 1960 to 1980
wdi=wdi[,!names(wdi) %in% remove1]

# Region country code
remove2=c("ARB","CEB","CSS","EAP","EAR","EAS","ECA","ECS","EUU","HIC","HPC","IBD","IBT","IDA","IDX",
          "LAC","LCN","LDC","LIC","LMC","LMY","MEA","MIC","MNA","NAC","OED","OSS","PRE","PSS","PST",
          "SAS","SSA","SSF","SST","TEA","TEC","TLA","TMN","TSA","UMC","WLD")

# Remove those regions, non-country data rows
wdi = wdi %>% filter(!Country.Code %in% remove2)

# Subset data for dependent variable poverty gap in $3.2 
poverty = wdi %>% filter(Indicator.Code %in% "SI.POV.LMIC.GP")

# Select those countries that have poverty data in 2012
pov12 = poverty %>% filter(!X2012 %in% NA)

pov12.list = pov12$Country.Code %>% table() %>% names()

# Subset wdi with those countries with poverty data in 2012
wdi.pov = wdi %>% filter(Country.Code %in% pov12.list)

# Transport the data frame between years and variables
wdi.povt=dcast(melt(as.data.table(wdi.pov), id.vars = c("Country.Name", 
                                                        "Country.Code","Indicator.Name", "Indicator.Code")), 
               Country.Name + Country.Code + variable ~ Indicator.Code, value.var = "value")

# Calculating missing values percentage
missing <- wdi.povt %>% summarize_all(funs(sum(is.na(.))/n()))
missing <- gather(missing, key="feature", value="missing_pct")
goodv <- filter(missing, missing_pct<0.25)

#Warren's code starts here
goodv=rbind(goodv,c("SI.POV.LMIC.GP", 10)) #add poverty metric so it doesn't get deleted
indicatornames=as.data.frame(goodv[,1])

# Subset by goodv
wdipov=subset(wdi.pov, Indicator.Code %in% indicatornames$`goodv[, 1]`)
wdipovt=dcast(melt(as.data.table(wdipov), id.vars = c("Country.Name", 
                                                      "Country.Code","Indicator.Name", "Indicator.Code")), 
              Country.Name + Country.Code + variable ~ Indicator.Code, value.var = "value", na.rm=T)


b2=as.data.frame(wdipovt)

na.aggregate(b2)

#Use the mean of each Indicator to impute NAs

for (i in 1:nrow(b2)) {
  for (j in 4:ncol(b2)) {
    if (is.na(b2[i,j])) {
      b2[i,j]=mean(b2[,j],na.rm=T)  
    }}}

hmm=lm(SI.POV.LMIC.GP~., data=b2[,-c(1,2,3)])
summary(hmm)

#####
## Gather all significant Indicators from the linear model. 
#####

significantIndicators=c("SI.POV.LMIC.GP","AG.LND.CREL.HA","AG.LND.CROP.ZS","AG.PRD.CREL.MT","AG.PRD.CREL.MT","AG.YLD.CREL.KG","BX.GSR.GNFS.CD",
                        "BX.GSR.MRCH.CD","EG.ELC.RNWX.KH","EN.ATM.NOXE.AG.KT.CE","EN.POP.DNST","EN.URB.LCTY","ER.FSH.AQUA.MT",
                        "ER.FSH.CAPT.MT","ER.FSH.PROD.MT","ER.FSH.PROD.MT","FI.RES.TOTL.MO","FP.CPI.TOTL","IT.CEL.SETS.P2",
                        "NE.CON.PRVT.KD","NE.CON.PRVT.KN","NE.CON.PRVT.ZS","NE.CON.TOTL.KN","NE.CON.TOTL.ZS","NE.DAB.TOTL.KN",
                        "NE.EXP.GNFS.KD.ZG","NE.EXP.GNFS.ZS","NE.GDI.TOTL.KN","NE.IMP.GNFS.KD.ZG","NE.IMP.GNFS.ZS","NV.AGR.TOTL.KD",
                        "NV.AGR.TOTL.ZS","NV.IND.MANF.ZS","NV.IND.TOTL.KD","NV.SRV.TOTL.CD")

#Create new matrix with only significant indicators

sigind=as.data.frame(significantIndicators)
#colnames(significantIndicators)=c("name")
wdipov2=subset(wdi.pov, Indicator.Code %in% sigind$significantIndicators)
wdipovt2=dcast(melt(as.data.table(wdipov2), id.vars = c("Country.Name", 
                                                        "Country.Code","Indicator.Name", "Indicator.Code")), 
               Country.Name + Country.Code + variable ~ Indicator.Code, value.var = "value", na.rm=T)

# Now use KNN imputation for a more accurate model. 

b1=kNN(wdipovt2[,-c(1:3)])
b3=wdipovt2
firstcols=wdipovt2[,1:3]
#pca=prcomp(b4, scale=T)
#summary(pca)
#names(pca)
b4=b1[,1:33]
b3[,4:36]=b4
b3[,1:3]=firstcols[,1:3]

#Which model makes best cross-sectional predictions?
control <- trainControl(method="cv", number=10)
metric <- "RMSE"
#fit.lm <- train(SI.POV.LMIC.GP~., data=b4, method="lm", metric=metric, trControl=control)
#fit.cart <- train(SI.POV.LMIC.GP~., data=b4, method="rpart", metric=metric, trControl=control)
#fit.knn <- train(SI.POV.LMIC.GP~., data=b4, method="knn", metric=metric, trControl=control)
#fit.svm <- train(SI.POV.LMIC.GP~., data=b4, method="svmRadial", metric=metric, trControl=control)
#fit.rf <- train(SI.POV.LMIC.GP~., data=b4, method="rf", metric=metric, trControl=control)
#results <- resamples(list(lm=fit.lm, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
#summary(results)
#RF, but we can't interrogate that
#SVM=svm(SI.POV.LMIC.GP~., data=b4, kernel="radial")
#summary(SVM)
#So we interrogate Linear model
linear=lm(SI.POV.LMIC.GP~., data=b4)

# Extract Highest statistical significance indicators
highcorr=c("SI.POV.LMIC.GP","AG.YLD.CREL.KG", "EN.ATM.NOXE.AG.KT.CE", "FP.CPI.TOTL", "IT.CEL.SETS.P2", "NE.CON.PRVT.ZS", "NE.EXP.GNFS.ZS", "NE.IMP.GNFS.ZS", "NV.AGR.TOTL.ZS", "NV.IND.MANF.ZS")
corrplot(cor(b4[,highcorr]))
#Eliminate highly corellated indicators
lol=lm(SI.POV.LMIC.GP~.,b4[,highcorr])
summary(lol)

##################
#The following are the chosen 5 indicators
##################
highcor2=c("SI.POV.LMIC.GP","EN.ATM.NOXE.AG.KT.CE", "IT.CEL.SETS.P2", 
           "NE.CON.PRVT.ZS", "NV.AGR.TOTL.ZS", "NV.IND.MANF.ZS")
corrplot(cor(b4[,highcor2]))
#Interrogate linear model for Rsquared
finlin=lm(SI.POV.LMIC.GP~.,b4[,highcor2])
summary(finlin)
#Create data matrix with columns by year 
b6=dcast(melt(as.data.table(wdipov2), id.vars = c("Country.Name", 
                                                  "Country.Code","Indicator.Name", "Indicator.Code")), 
         Country.Name + Country.Code + Indicator.Code ~ variable, value.var = "value", na.rm=T)
#Remove X's from year names
xremovenames=colnames(b6)
xremovenames
colnames(b6)=c("Country.Name", "Country.Code", "Indicator.Code", "1981","1982",         
               "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992",
               "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002",
               "2003", "2004", "2005", "2006", "2007","2008", "2009", "2010", "2011", "2012",
               "2013", "2014", "2015", "2016", "2017")
#Eliminate NA's
b7=kNN(b6)
indtrs=paste(b7[,3],b7[,2],sep=".")
b8=as.data.frame(t(b7[,4:40]))
colnames(b8)=indtrs

#Train VAR model and predict
b9=ts(b8, start=c(1981,1))

var=VAR(b9[,1:ncol(b9)/2], type="const")

fix(b9)
forecast1=predict(var, n.ahead = 5, ci=0.95)
summary(var)

frcst=data.frame(forecast1[[1]][1])
rmse=sqrt(frcst-data.frame(b8[,33:37]))^2 %>% colMeans(na.rm=T)%>%mean(na.rm=T)

plot(forecast1)
