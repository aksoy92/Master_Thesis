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

#open arrow
library(arrow)
options(scipen=999)


###### FUNCTIONS

#load median function: to receive observation number of median value
which.median = function(x) {
  if (length(x) %% 2 != 0) {
    which(x == median(x))
  } else if (length(x) %% 2 == 0) {
    a = sort(x)[c(length(x)/2, length(x)/2+1)]
    c(which(x == a[1]), which(x == a[2]))
  }
}

func_dfcreate<-function(row,hour){
  y<- data.frame(matrix(ncol = hour,nrow = row))
  x<- list()
  for (i in 1:(hour))
  {x<-append(x,paste("pr",i, sep = "_") )}
  colnames(y)<- x
  y
}


func_dfcreate_day<-function(row,day){
  hourname<-func_dfcreate(1,24)
  y<- data.frame(matrix(ncol = day*24,nrow = row))
  x<- list()
  for (i in 1:(day))
  {x<-append(x,paste(colnames(hourname),i, sep = "-") )}
  colnames(y)<- x
  y
}


day_frame <-function (day){
  df_day_frame_1<- data.frame(matrix(ncol =30 ,nrow =1 ))
  colnames(df_day_frame_1)<- colnames(df_day_coal)
  df_day_frame_2<-func_dfcreate_day(1,day)
  df_day_frame<-cbind(df_day_frame_1,df_day_frame_2)
}







###############################################################
### infra 2017 and 2018 are merged

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


################################################### STEP 1 CUT-OFF FOR COAL AND GAS   ########################
###################################################                   ########################

####################################
####################################

#### Cut-OFF FOR COAL 

####################################
####################################

#create index variable for each day (Date)
df_coal$index <- c(1,1+cumsum(diff(df_coal$Date)!=0))
df_coal$Datetime<-NULL
#######################################################
#######################################################
for (j in 1:24)# j is representing the hours which are taken into consideration for the analysis
{ 
  Dataset1 = subset (df_coal, Hour==j)
  Dataset1 <-Dataset1[order(Dataset1$Date, Dataset1$Price),]
  
  for(i in 1:730)# i represents every day of the sample
  {
    temp <- subset(Dataset1, Dataset1$index == i)
    
    #if no bids on day i, look  next day 
    if(is_empty(temp$Price) ){
      i =i+1
    } else { i=i}
    
    temp <- subset(Dataset1, Dataset1$index == i)
    temp <- temp[order(temp$Price),]
    
    x <- seq_along(temp$Price)
    y <- temp$Price
    dat <- data.frame(x,y)
    
    s <- smooth.Pspline(x, y)
    s2 <- predict(s, x, nderiv=1)
    s3 <- as.data.frame(s2)
    
    temp$ID <- seq.int(nrow(temp))
    s3$ID <- seq.int(nrow(s3))
    temp1=plyr::join(temp, s3, by="ID", type="left", match="all")
    
    upper  = s3 %>% filter(s3$ID >= max(which.median(s3$ID)))
    
    upper1  = upper %>% filter(upper$V1 > 0.5)
    max <- min(upper1$ID)
    
    lower  = s3 %>% filter(s3$ID < max(which.median(s3$ID)))
    lower<-lower[order(-lower$ID),]
    lower1  = lower %>% filter(lower$V1 > 0.5)
    
    min <- max(lower1$ID)
    temp2 <- temp1 %>% filter(temp1$ID > min)
    temp2 <- temp2 %>% filter(temp2$ID < max)
    
    if (i == 1) {Datasetfinal = temp2 }
    else Datasetfinal=rbind(Datasetfinal, temp2)}
  
  if (j==1){Datasetfinal1 = Datasetfinal}
  else Datasetfinal1 = rbind(Datasetfinal1, Datasetfinal)}


df_cut_coal <-Datasetfinal1

#######################################################
#######################################################

#  CUT-OFF FOR GAS

#######################################################
#######################################################


####clan df (from "for loop" for coal) 

Datasetfinal<- NULL
Datasetfinal1<-NULL

#create index variable for each day (Date)
df_gas$index <- c(1,1+cumsum(diff(df_gas$Date)!=0))
df_gas$Datetime<-NULL
#######################################################
#######################################################
for (j in 1:24)# j is representing the hours which are taken into consideration for the analysi
{ 
  Dataset1 = subset (df_gas, Hour==j)
  Dataset1 <-Dataset1[order(Dataset1$Date, Dataset1$Price),]
  
  for(i in 1:730)# i represents every day of the sample
  {
    temp <- subset(Dataset1, Dataset1$index == i)
    
    #if no bids on day i, look  next day 
    if(is_empty(temp$Price) ){
      i =i+1
    } else { i=i}
    
    
    temp <- subset(Dataset1, Dataset1$index == i)
    temp <- temp[order(temp$Price),]
    x <- seq_along(temp$Price)
    y <- temp$Price
    dat <- data.frame(x,y)
    s <- smooth.Pspline(x, y)
    s2 <- predict(s, x, nderiv=1)
    s3 <- as.data.frame(s2)
    temp$ID <- seq.int(nrow(temp))
    s3$ID <- seq.int(nrow(s3))
    temp1=plyr::join(temp, s3, by="ID", type="left", match="all")
    upper  = s3 %>% filter(s3$ID >= max(which.median(s3$ID)))
    upper1  = upper %>% filter(upper$V1 > 0.5)
    max <- min(upper1$ID)
    lower  = s3 %>% filter(s3$ID < max(which.median(s3$ID)))
    lower<-lower[order(-lower$ID),]
    lower1  = lower %>% filter(lower$V1 > 0.5)
    min <- max(lower1$ID)
    temp2 <- temp1 %>% filter(temp1$ID > min)
    temp2 <- temp2 %>% filter(temp2$ID < max)
    
    if (i == 1) {Datasetfinal = temp2 }
    else Datasetfinal=rbind(Datasetfinal, temp2)}
  
  if (j==1){Datasetfinal1 = Datasetfinal}
  else Datasetfinal1 = rbind(Datasetfinal1, Datasetfinal)}

#######################################################
#######################################################

df_cut_gas <-Datasetfinal1

#############  filtered values are assigned to dat_coal and dat_gas
df_notcut_gas<- df_gas
df_notcut_coal<-df_coal

####  !!!!!!!! !!!! can be Switched off cuttoff to not cutoff bids 
dat_gas<-df_cut_gas
dat_coal<-df_cut_coal




####### selets one  bid per hour

dat_coal<-data.frame(dat_coal %>% 
                       dplyr::select(Company,PP,coal,gas,year,month,day,Hour,Date,Price,Energy,MC,Clearing)%>%
                       dplyr::group_by(Company,PP,year,month,day,Hour,coal,gas)%>%
                       dplyr::summarise(  Price=median(Price,na.rm=TRUE),
                                          Energy=median(Energy,na.rm=TRUE),
                                          Clearing=mean(Clearing,na.rm=TRUE),
                                          MC=mean(MC,na.rm=TRUE))%>%
                       dplyr:: arrange(Company,PP,year,month,day,Hour))







dat_gas<-data.frame(dat_gas%>% dplyr::select(Company,PP,coal,gas,year,month,day,
                                             Hour,Date,Price,Energy,MC,Clearing)%>%
                      dplyr:: group_by(Company,PP,year,month,day,Hour,coal,gas)%>%
                      dplyr:: summarise(  Price=median(Price,na.rm=TRUE),
                                          Energy=median(Energy,na.rm=TRUE),
                                          Clearing=mean(Clearing,na.rm=TRUE),
                                          MC=mean(MC,na.rm=TRUE))%>%
                      dplyr:: arrange(Company,PP,year,month,day,Hour))




####### COAL- Creating Data Frame( df_day_coal)based of 24h for every day of 2018 and 2017

namedf<- func_dfcreate(1,24)

namedf$Company<-NA
namedf$PP<-NA
namedf$year<-NA
namedf$month<-NA
namedf$day<-NA
namedf$Mc<-NA
namedf<-namedf[,c(c(25:30),1:24)]
ndf<-namedf


###############



res_df<-data.frame(matrix(ncol=30))
colnames(res_df)<-colnames(namedf)
for ( c in as.character(unique(dat_coal$Company)) ){
  f_df1<-dat_coal %>%filter(Company==c)
  
  for ( p in as.character(unique(f_df1$PP)) ){
    f_df2<-f_df1 %>%filter(PP==p)
    
    for (y in 2017:2018){
      
      f_df3<-f_df2 %>% filter(year==y)
      
      
      for ( m in 1:12 ){
        
        df<-data.frame(matrix(ncol=30))
        colnames(df)<-colnames(ndf)
        
        df_d<-f_df3%>%filter(month==m)
        
        for ( d in 1:31 ){
          df_cd<-df_d %>%filter(day==d)
          
          for ( i in 1:24){
            df[d,(i+6)]<-df_cd[i,9]
            df[d,6]<-mean(df_cd[,12],na.rm=TRUE)
            
            df[d,1]<-c
            df[d,2]<-p
            df[d,3]<-y
            df[d,4]<-m
            df[d,5]<-d
          }
        }
        res_df<-rbind(res_df,df)
      }
    }
  } 
}



### Drop day 29-31 from month 2, 
## Drop day 31 fr0m month 4,6,9,11

#for 2018
res_df <-res_df[!(res_df$year==2018 & res_df$month==2 & res_df$day>28),]
res_df <-res_df[!(res_df$year==2018 & res_df$month==4 & res_df$day==31),]
res_df <-res_df[!(res_df$year==2018 & res_df$month==6 & res_df$day==31),]
res_df <-res_df[!(res_df$year==2018 & res_df$month==9 & res_df$day==31),]
res_df <-res_df[!(res_df$year==2018 & res_df$month==11 & res_df$day==31),]
#for 2017
res_df <-res_df[!(res_df$year==2017 & res_df$month==2 & res_df$day>28),]
res_df <-res_df[!(res_df$year==2017 & res_df$month==4 & res_df$day==31),]
res_df <-res_df[!(res_df$year==2017 & res_df$month==6 & res_df$day==31),]
res_df <-res_df[!(res_df$year==2017 & res_df$month==9 & res_df$day==31),]
res_df <-res_df[!(res_df$year==2017 & res_df$month==11 & res_df$day==31),]


# daily df_result (df_result) (drop Na in company caused by our codes ( not data ))

df_day_coal<-res_df %>% filter(!is.na(Company))

# convert NAN VAlues to Na in column Mc
df_day_coal$Mc <- sapply(df_day_coal$Mc, function(x) ifelse(is.nan(x), NA, x))





######  NA VALUES ##########################################################################################
# Rate  =   number of day they that have bidding / number of not  having bidding  day

na_month_coal  <-data.frame(df_day_coal %>% dplyr:: select(Company,PP,year,month,day,Mc) %>% 
                              dplyr::group_by(Company,PP,year,month) %>% filter(is.na(Mc)) %>% 
                              dplyr::summarise(na_day=n()) %>% arrange(Company,PP,year,month)) 

not_na_month_coal<-data.frame( df_day_coal %>% dplyr::select(Company,PP,year,month,day,Mc) %>%
                                 dplyr::group_by(Company,PP,year,month) %>% 
                                 dplyr::filter(!is.na(Mc)) %>% 
                                 dplyr::summarise(not_na_day=n()) %>%
                                 dplyr::arrange(Company,PP,year,month))







df_month_na_coal<- merge(na_month_coal,not_na_month_coal,by=c("Company","PP","year","month"),all.x=TRUE,all.y=TRUE)%>% arrange(Company,PP,year,month)

df_month_na_coal[is.na(df_month_na_coal$na_day),]$na_day<- 0
df_month_na_coal[is.na(df_month_na_coal$not_na_day),]$not_na_day<- 0

df_month_na_coal$rate_not_na<-df_month_na_coal$not_na_day/ (df_month_na_coal$na_day  + df_month_na_coal$not_na_day)

write_xlsx(df_month_na_coal,
           "C:\\Users\\Mr\\Desktop\\cuoff report\\df_month_na_coal.xlsx")

df_year_na_coal<-data.frame(df_month_na_coal %>%  group_by(Company,PP)%>% summarise(sum_na=sum(na_day),sum_not_na=sum(not_na_day)))

df_year_na_coal$rate_not_na<- df_year_na_coal$sum_not_na / (df_year_na_coal$sum_not_na + df_year_na_coal$sum_na)


write_xlsx(df_year_na_coal,
           "C:\\Users\\Mr\\Desktop\\cuoff report\\df_year_na_coal.xlsx")



##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  DAY ( WINDOW SIZE)  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#creating windows size!!!!!!!!! you can set it 15-30-45-60-- tt


day=90


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  DAY ( WINDOW SIZE)  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!



#### STEP 2-CoAL CREATING DATA FRAME According to windows size  


df_day_window<-day_frame(day)
df_Company_coal<-data.frame(matrix(ncol=ncol(df_day_window)))
colnames(df_Company_coal)<-colnames(df_day_window)

### Assigning values to the created dataframes.The dataframe will be adjusted according to sliding windpow

for ( c in as.character(unique(df_day_coal$Company)) ){
  f_df1<-df_day_coal %>%filter(Company==c)
  for ( p in as.character(unique(f_df1$PP)) ){
    f_df2<-f_df1 %>%filter(PP==p)
    f_df2$index<-1:nrow(f_df2)
    
    df_day_window<-day_frame(day)
    df_ml<-data.frame(matrix(ncol=ncol(df_day_window)))
    colnames(df_ml)<-colnames(df_day_window)
    
    for ( f in (day+1):730){
      df_ml[f,1:(ncol(f_df2)-1)]<-f_df2[f_df2$index==f,1:30]
    }
    df_ml<-df_ml[-c(1:(day)),] #drop na values
    
    for(r in 1:nrow(df_ml) ) {
      for(i in  1:day ){
        df_ml[r,(24*(i)+7):(24*(i)+30)]<-f_df2[f_df2$index==(r+day-i),7:30]
      }
    }
    df_Company_coal<-rbind(df_Company_coal,df_ml)
  }
}



######## Drop first row because of na  which is caused by codes 
df_Company_coal<- df_Company_coal[-1,]



##########################################                ####################################################   
##########################################               ####################################################
######################################### FEATURES COAL #####################################################
#########################################              ######################################################

#adding clearing price(max_median_min values of day)


df_clr <-unique(df_infra%>%select(year,month,day,Hour,Clearing))

df_clr$year<-factor(df_clr$year)
df_clr$month<-factor(df_clr$month)
df_clr$day<-factor(df_clr$day)
df_clr$Hour<-NULL

df_clr<-as.data.frame( unique(df_clr %>%group_by(year,month,day) %>% 
                                dplyr::summarise(
                                  min_clr=min(Clearing),
                                  median_clr=median(Clearing),
                                  max_clr=max(Clearing),
                                )))






df_ml_Company_coal<-  merge(x=df_Company_coal,y=df_clr,by=c("year","month","day"),all.x=TRUE)



#seasons
df_ml_Company_coal$seasons<-NA
df_ml_Company_coal<-df_ml_Company_coal %>% mutate( seasons = replace(df_ml_Company_coal$seasons,
                                                                     df_ml_Company_coal$month==12|
                                                                       df_ml_Company_coal$month==1|
                                                                       df_ml_Company_coal$month==2,"wint"))
df_ml_Company_coal<-df_ml_Company_coal %>% mutate( seasons = replace(df_ml_Company_coal$seasons,
                                                                     df_ml_Company_coal$month==3|
                                                                       df_ml_Company_coal$month==4|
                                                                       df_ml_Company_coal$month==5,"spr"))
df_ml_Company_coal<-df_ml_Company_coal %>% mutate( seasons = replace(df_ml_Company_coal$seasons,
                                                                     df_ml_Company_coal$month==6|
                                                                       df_ml_Company_coal$month==7|
                                                                       df_ml_Company_coal$month==8,"sum"))
df_ml_Company_coal<-df_ml_Company_coal %>% mutate( seasons = replace(df_ml_Company_coal$seasons,
                                                                     df_ml_Company_coal$month==9|
                                                                       df_ml_Company_coal$month==10|
                                                                       df_ml_Company_coal$month==11,"aut"))

#day type
#Adding date
df_ml_Company_coal$date <- as.Date(with(df_ml_Company_coal, paste(year, month, day,sep="-")), "%Y-%m-%d")
df_ml_Company_coal$date<-as.Date(df_ml_Company_coal$date)
#df_ml_Company_coal$year<-NULL


#Spain national holiday file downloaded from internet
#### attention 2 types of holiday 
holiday <- read_excel("holiday18.xlsx")

holiday$holiday<-as.Date(holiday$holiday)
df_ml_Company_coal$daytype<-NA

#df_gascoal[!(df_gascoal$Date  %in%  holiday$holiday),] //the code drops spain holiday

# bussiness day is assigned  to day type
df_ml_Company_coal<- df_ml_Company_coal%>% mutate( daytype = replace(df_ml_Company_coal$daytype,
                                                                     
                                                                     !(df_ml_Company_coal$date  %in%  holiday$holiday) 
                                                                     & isBizday( as.timeDate(df_ml_Company_coal$date)), "wrkday")  )

#holiday is assinged  to reaming NA values as "hlday"

df_ml_Company_coal<- df_ml_Company_coal%>% mutate( daytype = replace(df_ml_Company_coal$daytype,
                                                                     is.na(df_ml_Company_coal$daytype),
                                                                     "hlday"))







#### Convert  CATegoriCAL VARIBALE TO  Binary  var
binary_df_ml_Company_coal<- df_ml_Company_coal %>%
  mutate(
    winter = ifelse(seasons == "wint", 1, 0),
    summer = ifelse(seasons == "sum", 1, 0),
    autumn = ifelse(seasons == "aut", 1, 0),
    spring = ifelse(seasons == "spr", 1, 0),
    
    workday = ifelse(daytype == "wrkday", 1, 0),
    holiday = ifelse(daytype == "hlday", 1, 0) )


### DROP NOT  categorical features


# binary_df_ml_Company_coal$date<-NULL
binary_df_ml_Company_coal$seasons<-NULL
binary_df_ml_Company_coal$daytype<-NULL
binary_df_ml_Company_coal$index<-NULL
binary_df_ml_Company_coal$pr_median<-NULL
binary_df_ml_Company_coal$daytype<-NULL


binary_df_ml_Company_coal<-binary_df_ml_Company_coal  %>%
  arrange(Company,PP,year,month,day)



##########  SAME STEPS FOR GAS 
##########
##########
########### GAS
###########
############
############


####### GAS- Creating DF  based of 24h for every day of 2018

res_df<-data.frame(matrix(ncol=30))
colnames(res_df)<-colnames(namedf)
for ( c in as.character(unique(dat_gas$Company)) ){
  
  f_df1<-dat_gas %>%filter(Company==c)
  
  for ( p in as.character(unique(f_df1$PP)) ){
    f_df2<-f_df1 %>%filter(PP==p)
    
    for (y in 2017:2018){
      
      f_df3<-f_df2 %>% filter(year==y)
      
      
      for ( m in 1:12 ){
        
        df<-data.frame(matrix(ncol=30))
        colnames(df)<-colnames(ndf)
        
        df_d<-f_df3%>%filter(month==m)
        
        for ( d in 1:31 ){
          df_cd<-df_d %>%filter(day==d)
          
          for ( i in 1:24){
            df[d,(i+6)]<-df_cd[i,9]
            df[d,6]<-mean(df_cd[,12],na.rm=TRUE)
            df[d,1]<-c
            df[d,2]<-p
            df[d,3]<-y
            df[d,4]<-m
            df[d,5]<-d
          }
        }
        res_df<-rbind(res_df,df)
      }
    }
  } 
}

copy_resdf_gas<-res_df


### Drop day 29-31 from month 2, 
## Drop day 31 fr0m month 4,6,9,11

#for 2018
res_df <-res_df[!(res_df$year==2018 & res_df$month==2 & res_df$day>28),]
res_df <-res_df[!(res_df$year==2018 & res_df$month==4 & res_df$day==31),]
res_df <-res_df[!(res_df$year==2018 & res_df$month==6 & res_df$day==31),]
res_df <-res_df[!(res_df$year==2018 & res_df$month==9 & res_df$day==31),]
res_df <-res_df[!(res_df$year==2018 & res_df$month==11 & res_df$day==31),]
#for 2017
res_df <-res_df[!(res_df$year==2017 & res_df$month==2 & res_df$day>28),]
res_df <-res_df[!(res_df$year==2017 & res_df$month==4 & res_df$day==31),]
res_df <-res_df[!(res_df$year==2017 & res_df$month==6 & res_df$day==31),]
res_df <-res_df[!(res_df$year==2017 & res_df$month==9 & res_df$day==31),]
res_df <-res_df[!(res_df$year==2017 & res_df$month==11 & res_df$day==31),]


# daily df_result (df_result) (drop Na which is caused by our codes ( not data ))

df_day_gas<-res_df %>% filter(!is.na(Company))

# convert NAN VAlues to Na in column Mc
df_day_gas$Mc <- sapply(df_day_gas$Mc, function(x) ifelse(is.nan(x), NA, x))


# Rate  =   number of day they that have bidding / number of NA bidding  day

na_month_gas  <-data.frame(df_day_gas %>% dplyr:: select(Company,PP,year,month,day,Mc) %>% 
                             dplyr::group_by(Company,PP,year,month) %>% filter(is.na(Mc)) %>% 
                             dplyr::summarise(na_day=n()) %>% arrange(Company,PP,year,month)) 

not_na_month_gas<-data.frame( df_day_gas %>% dplyr::select(Company,PP,year,month,day,Mc) %>%
                                dplyr::group_by(Company,PP,year,month) %>% 
                                dplyr::filter(!is.na(Mc)) %>% 
                                dplyr::summarise(not_na_day=n()) %>%
                                dplyr::arrange(Company,PP,year,month))

df_month_na_gas<- merge(na_month_gas,not_na_month_gas,by=c("Company","PP","year","month"),all.x=TRUE,all.y=TRUE)%>% arrange(Company,PP,year,month)

df_month_na_gas[is.na(df_month_na_gas$na_day),]$na_day<- 0
df_month_na_gas[is.na(df_month_na_gas$not_na_day),]$not_na_day<- 0

df_month_na_gas$rate_not_na<-df_month_na_gas$not_na_day/ (df_month_na_gas$na_day  + df_month_na_gas$not_na_day)

write_xlsx(df_month_na_gas,
           "C:\\Users\\Mr\\Desktop\\cuoff report\\df_month_na_gas.xlsx")

df_year_na_gas<-data.frame(df_month_na_gas %>%  group_by(Company,PP)%>% summarise(sum_na=sum(na_day),sum_not_na=sum(not_na_day)))

df_year_na_gas$rate_not_na<- df_year_na_gas$sum_not_na / (df_year_na_gas$sum_not_na + df_year_na_gas$sum_na)


write_xlsx(df_year_na_gas,
           "C:\\Users\\Mr\\Desktop\\cuoff report\\df_year_na_gas.xlsx")





#### STEP 2-gas CREATING DATA FRAME According to windows size  


df_day_window<-day_frame(day)
df_Company_gas<-data.frame(matrix(ncol=ncol(df_day_window)))
colnames(df_Company_gas)<-colnames(df_day_window)

### Assigning values to the created dataframes.The dataframe will be adjusted according to sliding windpow

for ( c in as.character(unique(df_day_gas$Company)) ){
  f_df1<-df_day_gas %>%filter(Company==c)
  for ( p in as.character(unique(f_df1$PP)) ){
    f_df2<-f_df1 %>%filter(PP==p)
    f_df2$index<-1:nrow(f_df2)
    
    df_day_window<-day_frame(day)
    df_ml<-data.frame(matrix(ncol=ncol(df_day_window)))
    colnames(df_ml)<-colnames(df_day_window)
    
    for ( f in (day+1):730){
      df_ml[f,1:(ncol(f_df2)-1)]<-f_df2[f_df2$index==f,1:30]
    }
    df_ml<-df_ml[-c(1:(day)),] #drop na values
    
    for(r in 1:nrow(df_ml) ) {
      for(i in  1:day ){
        df_ml[r,(24*(i)+7):(24*(i)+30)]<-f_df2[f_df2$index==(r+day-i),7:30]
      }
    }
    df_Company_gas<-rbind(df_Company_gas,df_ml)
  }
}



######## Drop first row because of na caused by codes 
df_Company_gas<- df_Company_gas[-1,]




######################################### FEATURES gas

#adding clearing price

df_clr <-unique(df_infra%>%select(year,month,day,Hour,Clearing))

df_clr<-as.data.frame( unique(df_clr %>%group_by(year,month,day) %>% 
                                dplyr::summarise(
                                  min_clr=min(Clearing),
                                  median_clr=median(Clearing),
                                  max_clr=max(Clearing),
                                )))




df_ml_Company_gas<-  merge(x=df_Company_gas,y=df_clr,by=c("year","month","day"),all.x=TRUE)



#seasons
df_ml_Company_gas$seasons<-NA
df_ml_Company_gas<-df_ml_Company_gas %>% mutate( seasons = replace(df_ml_Company_gas$seasons,
                                                                   df_ml_Company_gas$month==12|
                                                                     df_ml_Company_gas$month==1|
                                                                     df_ml_Company_gas$month==2,"wint"))
df_ml_Company_gas<-df_ml_Company_gas %>% mutate( seasons = replace(df_ml_Company_gas$seasons,
                                                                   df_ml_Company_gas$month==3|
                                                                     df_ml_Company_gas$month==4|
                                                                     df_ml_Company_gas$month==5,"spr"))
df_ml_Company_gas<-df_ml_Company_gas %>% mutate( seasons = replace(df_ml_Company_gas$seasons,
                                                                   df_ml_Company_gas$month==6|
                                                                     df_ml_Company_gas$month==7|
                                                                     df_ml_Company_gas$month==8,"sum"))
df_ml_Company_gas<-df_ml_Company_gas %>% mutate( seasons = replace(df_ml_Company_gas$seasons,
                                                                   df_ml_Company_gas$month==9|
                                                                     df_ml_Company_gas$month==10|
                                                                     df_ml_Company_gas$month==11,"aut"))

#day type
#Adding date
df_ml_Company_gas$date <- as.Date(with(df_ml_Company_gas, paste(year, month, day,sep="-")), "%Y-%m-%d")
df_ml_Company_gas$date<-as.Date(df_ml_Company_gas$date)
#df_ml_Company_gas$year<-NULL


#Spain national holiday file downloaded from internet
#### attention 2 types of holiday 
holiday <- read_excel("holiday18.xlsx")

holiday$holiday<-as.Date(holiday$holiday)
df_ml_Company_gas$daytype<-NA

#df_gasgas[!(df_gasgas$Date  %in%  holiday$holiday),] //the code drops spain holiday

#assign bussiness day to day type
df_ml_Company_gas<- df_ml_Company_gas%>% mutate( daytype = replace(df_ml_Company_gas$daytype,
                                                                   !(df_ml_Company_gas$date  %in%  holiday$holiday) & isBizday( as.timeDate(df_ml_Company_gas$date)),
                                                                   "wrkday"))


#assing holiday value to reaming NA values as "hlday"

df_ml_Company_gas<- df_ml_Company_gas%>% mutate( daytype = replace(df_ml_Company_gas$daytype,
                                                                   is.na(df_ml_Company_gas$daytype),
                                                                   "hlday"))



#### Convert  CATegoriCAL VARIBALE TO  Binary  var
binary_df_ml_Company_gas<- df_ml_Company_gas %>%
  mutate(
    winter = ifelse(seasons == "wint", 1, 0),
    summer = ifelse(seasons == "sum", 1, 0),
    autumn = ifelse(seasons == "aut", 1, 0),
    spring = ifelse(seasons == "spr", 1, 0),
    
    workday = ifelse(daytype == "wrkday", 1, 0),
    holiday = ifelse(daytype == "hlday", 1, 0) )


### DROP NOT  RELEVANT COLUMNS


# binary_df_ml_Company_gas$date<-NULL
binary_df_ml_Company_gas$seasons<-NULL
binary_df_ml_Company_gas$daytype<-NULL
binary_df_ml_Company_gas$index<-NULL
binary_df_ml_Company_gas$pr_median<-NULL
binary_df_ml_Company_gas$daytype<-NULL


binary_df_ml_Company_gas<-binary_df_ml_Company_gas  %>%
  arrange(Company,PP,year,month,day)


######################### STEP 4 Merge two data frame

#merge
binary_df_ml_Company_coal$coal<-1
binary_df_ml_Company_coal$gas<-0

binary_df_ml_Company_gas$coal<-0
binary_df_ml_Company_gas$gas<-1

merged_df_ml<-rbind(binary_df_ml_Company_coal,binary_df_ml_Company_gas)








