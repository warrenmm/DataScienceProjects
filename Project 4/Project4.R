# P04
# Zillow Data Exploratory Analysis
# Adapted from https://www.kaggle.com/c/zillow-prize-1/kernels

#####Project 4 code begins on line 170.

setwd("C:/Users/Warren M/Desktop/DataScience/datasets/zillow prize project data")

#install.packages("DT")  
#install.packages("corrplot")  
#install.packages("leaflet")  
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)

properties <- read.csv('properties_2016.csv')
transactions <- read.csv('train_2016_v2.csv')
sample_submission <- read.csv('sample_submission.csv')

names(properties)
names(transactions)
names(sample_submission)

table(properties$yearbuilt)
hist(properties$yearbuilt)
table(transactions$transactiondate)

# Rename the variable names
# FunctionX(dataA) is the same as dataA %>% functionX

properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)

transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

# Convert dummary variables (Y and N) to (1 and 0)
properties <- properties %>% 
  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
         flag_tub = ifelse(flag_tub=="Y",1,0))

# Take a look at the data
properties <- properties %>% select(id_parcel, build_year, starts_with("area_"), 
                                    starts_with("num_"), starts_with("flag_"), starts_with("region_"), everything())
datatable(head(properties,100), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
datatable(head(transactions,100), style="bootstrap", class="table-condensed", options = list(dom = 'tp'))

# Take a look at the transaction data
tmp <- transactions %>% mutate(year_month = make_date(year=year(date),month=month(date)))
tmp %>% 
  group_by(year_month) %>% count() %>% 
  ggplot(aes(x=year_month,y=n)) +
  geom_bar(stat="identity", fill="red")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-10-01"))),size=2)

# Distribution of Zestaimate's forecast errors (log rror)
# logerror: log(Zestimate) - log(Saleprice). So a positive logerror means Zestimate is overestimating the Saleprice, 
# a negative logerror means that Zestimate is underestimating Saleprice. 
# absolute logerror: a small value means that log(Zestimate) is close to log(Saleprice). 
# So, Zestimate predictions are close to Saleprice.

transactions %>% 
  ggplot(aes(x=logerror)) + 
  geom_histogram(bins=400, fill="red")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(-0.5,0.5))

# Absolute logerror
transactions <- transactions %>% mutate(abs_logerror = abs(logerror))
# it is the same as: transactions$abs_logerror=abs(transactions$logerror)

transactions %>% 
  ggplot(aes(x=abs_logerror)) + 
  geom_histogram(bins=400, fill="orange")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(0,0.5))

# How does log error change with time
transactions %>% 
  mutate(year_month = make_date(year=year(date),month=month(date)) ) %>% 
  group_by(year_month) %>% summarize(mean_logerror = mean(logerror)) %>% 
  ggplot(aes(x=year_month,y=mean_logerror)) + 
  geom_line(size=1.5, color="red")+geom_point(size=5, color="red")+theme_bw()

# Missing values management
missing_values <- properties %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

good_features <- filter(missing_values, missing_pct<0.25)

# Correlation with absolute logerror
vars <- good_features$feature[str_detect(good_features$feature,'num_')]
cor_tmp <- transactions %>% left_join(properties, by="id_parcel") 
tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))
corrplot(cor(tmp, use="complete.obs"),type="lower")

vars <- good_features$feature[str_detect(good_features$feature,'area_')]
tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))
corrplot(cor(tmp, use="complete.obs"), type="lower")

vars <- setdiff(good_features$feature[str_detect(good_features$feature,'tax_')],c("tax_delinquency","tax_year"))
tmp <- cor_tmp %>% select(one_of(c(vars,"abs_logerror")))
corrplot(cor(tmp, use="complete.obs"), type="lower")

# Correlation with logerror
vars <- good_features$feature[str_detect(good_features$feature,'')]
cor_tmp <- transactions %>% left_join(properties, by="id_parcel") 
tmp2 <- cor_tmp %>% select(one_of(c(vars,"logerror")))
#tmp2 <- tmp2%>% mutate(region_country = factor(region_country))
rmv=c("flag_tub","flag_fireplace","area_live_finished","num_bathroom_calc","num_bath","tax_property","id_parcel", "fips", "latitude", "longitude", "zoning_landuse_county", "zoning_property", "rawcensustractandblock", "region_city", "region_zip", "censustractandblock","tax_year","tax_building","tax_land")
tmp=tmp2[,!names(tmp2) %in% rmv]
tmp=tmp%>% mutate(abs_logerror = abs(logerror))
corrplot(cor(tmp, use="complete.obs"),type="lower")
cor(tmp, use="complete.obs")
str(tmp)
tmp$zoning_landuse=factor(tmp$zoning_landuse)
tmp$region_county=factor(tmp$region_county)
str(tmp)
fit1=lm(logerror~.-abs_logerror, data=tmp)
summary(fit1)
library(leaps)
regsub1=regsubsets(logerror~.-abs_logerror,data=tmp)
summary(regsub1)
fit2=lm(abs_logerror~.-logerror, data=tmp)
summary(fit2)
regsub2=regsubsets(abs_logerror~.-logerror,data=tmp)
summary(regsub2)

#Area total calc and tax delinquency contribute toward absolute log error 
#and log error.  Zillow should see if reducing the weight of these variables
#makes the Zestimate more or less accurate, and change their model accordingly.

#Build year is a significant contributor only to abs_logerror. This means 
#that the build year affects the log error both positively and negatively, 
#which means that it is an unreliable predictor in the current data. Its
#weight in the Zestimate should be reduced.
