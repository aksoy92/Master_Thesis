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
library(writexl)

#open arrow
library(arrow)
options(scipen=999)
###### Dummy variable for companies
options(scipen=999)


######################################################################################## 
######################################################################################## 
################################ !!!!!!!!!!!!!!!!!!  NOTE 
######################################################################################## 

# The prepared data put into Xgboost model. You can directly run it with supportive feature
# You can select only bids with the code in line  134 !!!! It's in comment line 













merged_df_ml<-rbind(binary_df_ml_Company_coal,binary_df_ml_Company_gas)







## adding infra quantity

df_Quantity<-as.data.frame(df_infra%>%select(year,month,day,Company,PP,Infra)%>%
                             group_by(year,month,day,Company,PP) %>% 
                             dplyr::summarise(
                               min_Quantity=min(Infra),
                               median_Quantity=median(Infra),
                               max_Quantity=max(Infra),
                             ))
merged_df_ml<-  merge(x=merged_df_ml,y=df_Quantity,by=c("year","month","day","Company","PP"),all.x=TRUE)

##### NOTE eventhough rows with na MC (no resonable bidding in that day  or no bidding ) can  have infra quantity  (our data filtered)
###### They will be removefd with drop na in MC


df_mrgd_ml<- merged_df_ml




#df_mrgd_ml$Company<-factor(df_mrgd_ml$Company)






#create a function that adding Company's dummy (one-hot-coding )

#list_Company=list("Endesa","EDP","REN","Naturgy", "Iberdrola", "Viesgo" , "Arclight", "Alpiq","BP","Engie")
#for ( i in list_Company){
#    df_mrgd_ml<- df_mrgd_ml %>% mutate( as.name(i) = ifelse(Company == i, 1, 0) )
#    }

#df_mrgd_ml$Company<-NULL

df_mrgd_ml<- df_mrgd_ml %>% mutate( Endesa = ifelse(Company == "Endesa", 1, 0),
                                    EDP = ifelse(Company == "EDP", 1, 0),
                                    REN = ifelse(Company == "REN", 1, 0),
                                    Naturgy = ifelse(Company == "Naturgy", 1, 0),
                                    Iberdrola = ifelse(Company == "Iberdrola", 1, 0),
                                    Viesgo = ifelse(Company == "Viesgo", 1, 0),
                                    Arclight = ifelse(Company == "Arclight", 1, 0),
                                    Alpiq = ifelse(Company == "Alpiq", 1, 0),
                                    BP = ifelse(Company == "BP", 1, 0),
                                    Engie = ifelse(Company == "Engie", 1, 0),
                                    Cepsa = ifelse(Company == "Cepsa", 1, 0),
                                    Ignis = ifelse(Company == "Ignis", 1, 0),)


df_mrgd_ml<- df_mrgd_ml %>% mutate( year_2017 = ifelse(year == 2017, 1, 0),
                                    year_2018 = ifelse(year == 2018, 1, 0))





#Drop char becaue of XGBOOST do not accept char variables 
df_mrgd_ml$PP<-NULL
df_mrgd_ml$Company<-NULL
df_mrgd_ml$year<-NULL



#Drop MC =Na for Today 
df_mrgd_ml<-  df_mrgd_ml[complete.cases(df_mrgd_ml$Mc),]


####  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! to select  ONLY BIDS,MC and DATE info (ONLY PRICE  for machnie learning)
######################################
######################################

#it can be swithced here !!!!!!!!!!!!!!!!!!!

#df_mrgd_ml <- df_mrgd_ml[,grepl(("pr_|Mc|date"),names(df_mrgd_ml))]










#TRAIN AND TEST ## 
#Random split
#is.na(df_mrgd_ml$Mc)
#set.seed(3455)
#train_indeks <- createDataPartition(df_mrgd_ml$Mc, p = .7, list = FALSE, times = 1)

#Train <- df_mrgd_ml[train_indeks, ]
#Test <- df_mrgd_ml[-train_indeks, ]


## for 90 day

Train <-as.data.frame(df_mrgd_ml %>%filter(date <"2018-07-01" ))
Test <-as.data.frame(df_mrgd_ml %>%filter(date  >="2018-10-01"))
#Test <-df_mrgd_ml




### col names 

names(Test ) <- gsub(x = names(Test), pattern = "\\-", replacement = "_")
names(Train ) <- gsub(x = names(Train), pattern = "\\-", replacement = "_")


Train$month<-NULL
Train$day<-NULL
Train$date<-NULL

Test$month<-NULL
Test$day<-NULL
Test$date<-NULL

##Preparation  labels for Train and TEst set 


train_x <- Train %>% dplyr::select(-Mc)
train_y <- Train$Mc
test_x <- Test %>% dplyr::select(-Mc)
test_y <- Test$Mc



###### xgboost 
#install.packages("drat",repos="https://cran.rstudio.com")
#install.packages("xgboost",repos="http://dmlc.ml/drat/",type="source")
drat:::addRepo("dmlc")
library("xgboost")





### Model

dtrain <- xgb.DMatrix(data = as.matrix(train_x), label = train_y)
dtest <- xgb.DMatrix(data = as.matrix(test_x), label = test_y)





#xgboost_tree 

xgboost_fit_tree<-xgboost(data = as.matrix(train_x),
                          label = train_y, 
                          booster = "gbtree",
                          nrounds=1000, 
                          max_depth= 6, 
                          eta= 0.1,     
                          gamma= 0,
                          colsample_bytree=0.6, 
                          min_child_weight=3, 
                          subsample= 0.75,
                          alpha =1,
                          lambda=1)

## TEST Tree



df_result <-defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgboost_fit_tree, as.matrix(test_x))))




defaultSummary(data.frame(obs = test_y, 
                          pred = predict(xgboost_fit_tree, as.matrix(test_x))))




df_result <- cbind(Value = rownames(df_result), df_result)

Model_result  <- data.frame(matrix(ncol=3,nrow=1))
colnames(Model_result)  <-    rownames(df_result)

Model_result[,1]<-df_result[1,]
Model_result[,2]<-df_result[2,]
Model_result[,3]<-df_result[3,]


imp_matris_tree <- xgb.importance(model = xgboost_fit_tree)




write_xlsx(data.frame(Model_result),
           "C:\\Users\\woz4\\Desktop\\ml_results\\defaultSummary_onlyPrice.xlsx")


write_xlsx(imp_matris_tree,
           "C:\\Users\\woz4\\Desktop\\ml_results\\imp_matris_tree_onlyprice.xlsx")



##### Plot for feature importance

#It can be seen top 50,100,150...... change the rank <=

xgbImp1 <- imp_matris_tree %>% mutate(rank = dense_rank(desc(Gain)))

xgbImp1$Feature<- factor(xgbImp1$Feature)

ggplot(data=xgbImp1[which(xgbImp1$rank <= 50),], aes(x = reorder(Feature, -Gain), y = Gain)) +
  geom_bar(stat="identity",aes(fill=Feature)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none") +
  labs(title = "XG Boosted Feature Importance (Top50)", x = "Features", y = "Information Gain")







