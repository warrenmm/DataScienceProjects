library(readxl)
setwd("C:/Users/Warren M/Desktop/DataScience/datasets")
zcWork<- read_excel("P01_LA zipcode payroll.xlsx")  
typeof(zcWork[1,1])
colnames(zcWork)[colnames(zcWork)=="Zip Code"]="zc"
zipCodePercentages=matrix(nrow=3592,ncol=5)


for (num in 1:3592){
  numZip=90000+num
  currentZip=as.list(numZip)
  oneByOne1 <- subset(zcWork, zc==currentZip)
  oneByOne2=as.data.frame(oneByOne1[,1:6])
  oneByOne3=as.numeric(oneByOne2[,5])
  oneByOne4=oneByOne2
  oneByOne4[,5]=c(oneByOne3)
  oneByOne6=na.omit(oneByOne4)
  sum1=oneByOne6[,5]
  sum2=sum(sum1)
  info1 = subset(oneByOne6, Industry == "Information")
  info2=info1[1,5]
  sci1 = subset(oneByOne6, Industry == "Professional, Scientific, & Technical Skills")
  sci2=sci1[1,5]
  if (is.na(sci2)){
    sci2=0
    a=1
  }else{
    a=2
    
  }
  if (is.na(info2)){
    info2=0
    b=1
  }else{
    b=2  
  }
  techJobs=info2+sci2
  percentTechJobs=techJobs/sum2*100
  zipCodePercentages[num,2]=percentTechJobs
  zipCodePercentages[num,1]=numZip
  zipCodePercentages[num,3]=info2
  zipCodePercentages[num,4]=sci2
  zipCodePercentages[num,5]=sum2
  }
  
colnames(zipCodePercentages) <- c("Zip_Code", "Percent_(info+tech)/total_Jobs", "Information_Jobs", "Science_Professional_&_Technical_Jobs", "Total_Employment") 
finalData=na.omit(zipCodePercentages)
#install.packages("xlsx")
library(xlsx)
write.xlsx(finalData, "project1b.xlsx")
