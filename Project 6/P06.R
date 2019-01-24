##################################################################################################
# P06
# Script Answer to Project 6, UCLA Extension Data Science Intensive
# 11/9/2018 by William Yu
##################################################################################################
setwd("C:/Users/Warren M/Desktop/DataScience/datasets")
#install.packages("DataCombine")
library(DataCombine)
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


goodv=rbind(goodv,c("SI.POV.LMIC.GP", 10))
indicatornames=as.data.frame(goodv[,1])
#indicatorname[nrow(indicatorname),]=as.character("SI.POV.LMIC.GP"))

# Subset by goodv
wdipov=subset(wdi.pov, Indicator.Code %in% indicatornames$`goodv[, 1]`)
wdipovt=dcast(melt(as.data.table(wdipov), id.vars = c("Country.Name", 
                        "Country.Code","Indicator.Name", "Indicator.Code")), 
              Country.Name + Country.Code + variable ~ Indicator.Code, value.var = "value", na.rm=T)

b2=as.data.frame(wdipovt)

na.aggregate(b2)

for (i in 1:nrow(b2)) {
  for (j in 4:ncol(b2)) {
    if (is.na(b2[i,j])) {
      b2[i,j]=mean(b2[,j],na.rm=T)     #as.numeric(b2[i,5]))/as.numeric(b2[i,6])))
    }}}

hmm=lm(SI.POV.LMIC.GP~., data=b2[,-c(1,2,3)])
summary(hmm)

significantIndicators=c("AG.LND.CREL.HA","AG.LND.CROP.ZS","AG.PRD.CREL.MT","AG.PRD.CREL.MT","AG.YLD.CREL.KG","BX.GSR.GNFS.CD",
"BX.GSR.MRCH.CD","EG.ELC.RNWX.KH","EN.ATM.NOXE.AG.KT.CE","EN.POP.DNST","EN.URB.LCTY","ER.FSH.AQUA.MT",
"ER.FSH.CAPT.MT","ER.FSH.PROD.MT","ER.FSH.PROD.MT","FI.RES.TOTL.MO","FP.CPI.TOTL","IT.CEL.SETS.P2",
"NE.CON.PRVT.KD","NE.CON.PRVT.KN","NE.CON.PRVT.ZS","NE.CON.TOTL.KN","NE.CON.TOTL.ZS","NE.DAB.TOTL.KN",
"NE.EXP.GNFS.KD.ZG","NE.EXP.GNFS.ZS","NE.GDI.TOTL.KN","NE.IMP.GNFS.KD.ZG","NE.IMP.GNFS.ZS","NV.AGR.TOTL.KD",
"NV.AGR.TOTL.ZS","NV.IND.MANF.ZS","NV.IND.TOTL.KD","NV.SRV.TOTL.CD")
sigind=as.data.frame(significantIndicators)
colnames(significantIndicators)=c("name")
wdipov2=subset(wdi.pov, Indicator.Code %in% sigind$significantIndicators)
wdipovt2=dcast(melt(as.data.table(wdipov2), id.vars = c("Country.Name", 
                                                      "Country.Code","Indicator.Name", "Indicator.Code")), 
              Country.Name + Country.Code + variable ~ Indicator.Code, value.var = "value", na.rm=T)



library(bnstruct)
library(mice)
library(VIM)
wdipovt3=regressionImp(SI.POV.LMIC.GP~., data=wdipovt)
write.csv(wdipovt,"p6d.csv")
wdi
wdipovd=na.rm    
a=mice(wdipovt,m=1,method="cart")


summary(hmm)
str(wdipovt)
a=as.data.frame(wdipovt)


NAs=as.data.frame(map_dbl(b,~sum(is.na(.))/nrow(b)))
natf=as.data.frame(matrix(data=NA, nrow(NAs),1))
for (i in 1:nrow(NAs)) {
  if (NAs[i,]<=.6) {
    natf[i,]=1
  } else { natf[i,]=0}}
NAs[,2]=b2[,1]



##Prepare to erase rows w/ too many NAs 
z=rep(NA, nrow(a))
nrow(b2)
headers=b2[,1:3]
##Set NAs as row average and save income values

income=as.data.frame(rep(1,nrow(b2),3))

for (i in 1:ncol(b2)) {
  b2[,i]=as.numeric(as.character(b2[,i]))
}


b1=b2

for (i in 1:nrow()) {
  for (j in 1:ncol(b2)) {
    if (is.na(b2[i,j])) {
      b2[i,j]=(as.numeric(b2[i,5]))#/as.numeric(b2[i,6])))
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

#remove those





