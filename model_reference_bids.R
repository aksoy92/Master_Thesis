zrequire('timeDate')
require(readxl)
require(ggplot2)
require(dplyr)
require(lubridate)
require(naniar)
require(caret)
require(lattice)
require(hexbin)
require(corrplot)
require(scales)
require(tidyverse)
library(Dict)
require(caTools)
require(gbm)
require(TTR)
library(reshape)
library(timeDate)
library(caret)
library(pspline)
library(plyr)
library(dplyr)
install.packages('bizdays')
library(bizdays)
install.packages("Metrics")
library(Metrics)

#open arrow
library(arrow)
options(scipen=999)







###############################################################
df_infra<-rbind(infra17,infra)

##reshape Infra
df_infra$month<-month(as.POSIXlt(df_infra$Date, format="%Y/%m/%d"))
df_infra$day<-day(as.POSIXlt(df_infra$Date, format="%Y/%m/%d"))
df_infra$year<-year(as.POSIXlt(df_infra$Date, format="%Y/%m/%d"))


##fuel dataframe
df_gascoal<-df_infra%>%filter(Fuel=="Coal"|Fuel=="Natural Gas")
df_gascoal$Company<-factor(df_gascoal$Company)
df_gascoal$year<-factor(df_gascoal$year)
df_gascoal$month<-factor(df_gascoal$month)
df_gascoal$day<-factor(df_gascoal$day)
df_gascoal$Hour<-factor(df_gascoal$Hour)
df_gascoal$PP<-factor(df_gascoal$PP)

#binary fuel
df_gascoal <- df_gascoal %>%
  mutate(coal = ifelse(Fuel == "Coal", 1, 0),
         gas = ifelse(Fuel == "Natural Gas", 1, 0))


df_coal<- df_gascoal%>%filter(coal==1)
df_gas<- df_gascoal%>%filter(coal==0)



df_coal$daytype<-NA
#assign bussiness day to day type

df_coal$date <- as.Date(with(df_coal, paste(year, month, day,sep="-")), "%Y-%m-%d")
df_coal$date<-as.Date(df_coal$date)

df_coal<- df_coal%>% mutate( daytype = replace(df_coal$daytype,(isBizday( as.timeDate(df_coal$date))),"wrkday"))
df_coal<- df_coal%>% mutate( daytype = replace(df_coal$daytype,is.na(df_coal$daytype),"weekend"))

#exclude weekends #only peak-hours
df_coal$Hour<- as.numeric(df_coal$Hour)
df_coal <-df_coal%>%filter((daytype=="wrkday") & (Hour<=23) & (Hour>=8))

#Price larger than 15 and smaller than 3 times MC

df_coal <-df_coal%>%filter(Price>=15 & Price <= MC*3)


#Select matched Bids
df_coal<- df_coal%>%filter(Matched=="C")


### df for refference bids

df_ref_coal<-df_coal %>% select(date,daytype,Hour,Company,PP,coal,gas,year,month,day,Price,Matched,MC)


#Creting index according to day 

empty_df<-data.frame(matrix(ncol=ncol(df_ref_coal)))
colnames(empty_df)<-colnames(df_ref_coal)
empty_df$index<-NA
empty_df$date<-as.Date(empty_df$date)

for ( c in as.character(unique(df_ref_coal $Company)) ){
f_df1<-df_ref_coal  %>%filter(Company==c)
for ( p in as.character(unique(f_df1$PP)) ){
f_df2<-f_df1 %>%filter(PP==p)
f_df2$index <- c(1,1+cumsum(diff(f_df2$date)!=0))

}
empty_df<-rbind(empty_df,f_df2)
}

df_ref_coal <-empty_df

## drop NA caused by codes

df_ref_coal <-na.omit(df_ref_coal)


## s

df_ref_coal<- df_ref_coal%>% arrange(Company,PP,date,Price,index)

# Mean and Meadian from recent 90 days 

df_ref_coal$Mean<- NA
df_ref_coal$Median<- NA


  


empty_df<-data.frame(matrix(ncol=ncol(df_ref_coal)))
colnames(empty_df)<-colnames(df_ref_coal)
empty_df$date<-as.Date(empty_df$date)



for ( c in as.character(unique(df_ref_coal$Company)) ){
  f_df1<-df_ref_coal%>% filter(Company==c)
  for ( p in as.character(unique(f_df1$PP)) ){
    f_df2<-f_df1 %>%filter(PP==p)
    
    f_df2<- f_df2%>% arrange(Company,PP,date,Price,index)

   for(i in min(f_df2$index):max(f_df2$index)){
     
     if( 90 >= ( as.numeric( unique(f_df2[f_df2$index==i,]$date)-min(f_df2$date)) )){
       f_df2[f_df2$index==i,]$Mean=mean(f_df2[ f_df2$index<=i,]$Price) 
       f_df2[f_df2$index==i,]$Median=median(f_df2[ f_df2$index<=i,]$Price)
     }else{
       f_df2[f_df2$index==i,]$Mean=mean(f_df2[unique(f_df2[f_df2$index==i,]$date)-days(90) :unique(f_df2[f_df2$index==i,]$date) ,]$Price,
                                        na.rm = TRUE) 
       f_df2[f_df2$index==i,]$Median=median(f_df2[unique(f_df2[f_df2$index==i,]$date)-days(90) :unique(f_df2[f_df2$index==i,]$date) ,]$Price,
                                            na.rm = TRUE)}
   }
    empty_df<-rbind(empty_df,f_df2)
    
  }
  
}

#drop na caused by codes
empty_df <- na.omit(empty_df)


df_ref_bids_coal <- empty_df

df_ref_bids_coal$ref_bid<-NA


#selecting the minimum of meadian and mean
df_ref_bids_coal <- transform(df_ref_bids_coal,ref_bid = pmin(Mean, Median))
  
df_day_ref_bids_coal <- unique(df_ref_bids_coal %>% select(date,year,month,day,Company,PP,coal,gas,MC,Mean,Median,ref_bid))


#################
#################    GAS
################# 
################# 




df_gas$daytype<-NA
#assign bussiness day to day type

df_gas$date <- as.Date(with(df_gas, paste(year, month, day,sep="-")), "%Y-%m-%d")
df_gas$date<-as.Date(df_gas$date)

df_gas<- df_gas%>% mutate( daytype = replace(df_gas$daytype,(isBizday( as.timeDate(df_gas$date))),"wrkday"))
df_gas<- df_gas%>% mutate( daytype = replace(df_gas$daytype,is.na(df_gas$daytype),"weekend"))

#exclude weekends #only peak-hours
df_gas$Hour<- as.numeric(df_gas$Hour)
df_gas <-df_gas%>%filter((daytype=="wrkday") & (Hour<=23) & (Hour>=8))

#Price larger than 15 and smaller than 3 times MC

df_gas <-df_gas%>%filter(Price>=15 & Price <= MC*3)


#Select matched Bids
df_gas<- df_gas%>%filter(Matched=="C")


### df for refference bids

df_ref_gas<-df_gas %>% select(date,daytype,Hour,Company,PP,coal,gas,year,month,day,Price,Matched,MC)


#Creting index according to day 

empty_df<-data.frame(matrix(ncol=ncol(df_ref_gas)))
colnames(empty_df)<-colnames(df_ref_gas)
empty_df$index<-NA
empty_df$date<-as.Date(empty_df$date)

for ( c in as.character(unique(df_ref_gas $Company)) ){
  f_df1<-df_ref_gas  %>%filter(Company==c)
  for ( p in as.character(unique(f_df1$PP)) ){
    f_df2<-f_df1 %>%filter(PP==p)
    f_df2$index <- c(1,1+cumsum(diff(f_df2$date)!=0))
    
  }
  empty_df<-rbind(empty_df,f_df2)
}

df_ref_gas <-empty_df

## drop NA caused by codes

df_ref_gas <-na.omit(df_ref_gas)


## s

df_ref_gas<- df_ref_gas%>% arrange(Company,PP,date,Price,index)

# Mean and Meadian from recent 90 days 

df_ref_gas$Mean<- NA
df_ref_gas$Median<- NA





empty_df<-data.frame(matrix(ncol=ncol(df_ref_gas)))
colnames(empty_df)<-colnames(df_ref_gas)
empty_df$date<-as.Date(empty_df$date)



for ( c in as.character(unique(df_ref_gas$Company)) ){
  f_df1<-df_ref_gas%>% filter(Company==c)
  for ( p in as.character(unique(f_df1$PP)) ){
    f_df2<-f_df1 %>%filter(PP==p)
    
    f_df2<- f_df2%>% arrange(Company,PP,date,Price,index)
    
    for(i in min(f_df2$index):max(f_df2$index)){
      
      if( 90 >= ( as.numeric( unique(f_df2[f_df2$index==i,]$date)-min(f_df2$date)) )){
        f_df2[f_df2$index==i,]$Mean=mean(f_df2[ f_df2$index<=i,]$Price) 
        f_df2[f_df2$index==i,]$Median=median(f_df2[ f_df2$index<=i,]$Price)
      }else{
        f_df2[f_df2$index==i,]$Mean=mean(f_df2[unique(f_df2[f_df2$index==i,]$date)-days(90) :unique(f_df2[f_df2$index==i,]$date) ,]$Price,
                                         na.rm = TRUE) 
        f_df2[f_df2$index==i,]$Median=median(f_df2[unique(f_df2[f_df2$index==i,]$date)-days(90) :unique(f_df2[f_df2$index==i,]$date) ,]$Price,
                                             na.rm = TRUE)}
    }
    empty_df<-rbind(empty_df,f_df2)
    
  }
  
}

#drop na caused by codes
empty_df <- na.omit(empty_df)


df_ref_bids_gas <- empty_df

df_ref_bids_gas$ref_bid<-NA


#selecting the minimum of meadian and mean
df_ref_bids_gas <- transform(df_ref_bids_gas,ref_bid = pmin(Mean, Median))

df_day_ref_bids_gas <- unique(df_ref_bids_gas %>% select(date,year,month,day,Company,PP,coal,gas,MC,Mean,Median,ref_bid))


################
################
################

df_ref_2years_coalandgas  <- rbind(df_day_ref_bids_coal,df_day_ref_bids_gas)


######  Calculate maen absolute and root mean squred error for 
summary(df_ref_2years_coalandgas$Company)

df_set_test_for_ML<- df_ref_2years_coalandgas %>% 
                    select(-month,-day,-year,-Mean,-Median)%>% 
                    filter(date  >="2018-10-01")

library(Metrics)

mae(df_ref_2years_coalandgas$MC, df_ref_2years_coalandgas$ref_bid)
rmse(df_ref_2years_coalandgas$MC, df_ref_2years_coalandgas$ref_bid)

mae(df_set_test_for_ML$MC, df_set_test_for_ML$ref_bid)
rmse(df_set_test_for_ML$MC, df_set_test_for_ML$ref_bid)


nrow(df_ref_2years_coalandgas)


  