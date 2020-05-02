#import store data
library(readxl)
store<-read_excel("Store.xlsx")

#import transnumber by store information
trans_c_store_new<-read.csv("result_coupon.csv")
trans_all_store_new<-read.csv("result_all.csv")
names(trans_c_store_new)[1]<-"STORE_NUM"
names(trans_all_store_new)[1]<-"STORE_NUM"
#merge multiple coupon and adjust the format of coupon dataset
library(tidyverse)
library(dplyr)
trans_c_store_new2<-trans_c_store_new%>%group_by(STORE_NUM,YEAR,MONTH,CouponType)%>%
  summarise(Sales_C=sum(SALES_C),Cogs_C=sum(COGS_C),UnitSold_C=sum(UNITSOLD_C),Trans_Num_C=sum(TRANSNUMBER_C),Margin_C=sum(Margin_C))

#adjust the format of coupon dataset
trans_c_store_new3<-pivot_wider(trans_c_store_new2,id_cols = c('STORE_NUM','MONTH','YEAR'),names_from = CouponType,values_from = c('Sales_C','Cogs_C',
                                              'UnitSold_C','Trans_Num_C','Margin_C'))
#create the dataset which didn't have specific coupon type
trans_c_store_all<-trans_c_store_new%>%group_by(STORE_NUM,YEAR,MONTH)%>%
  summarise(Sales_C=sum(SALES_C),Cogs_C=sum(COGS_C),UnitSold_C=sum(UNITSOLD_C),Trans_Num_C=sum(TRANSNUMBER_C),Margin_C=sum(Margin_C))

#join dataset which didn't have specific coupon type, has specific coupon type and all transaction
trans_store_new1<-merge(trans_c_store_all,trans_all_store_new,by=c('STORE_NUM','MONTH','YEAR'))
trans_store_new2<-merge(trans_c_store_new3,trans_store_new1,by=c('STORE_NUM','MONTH','YEAR'))
DV_pre<-merge(trans_store_new2,store,by.x = "STORE_NUM",by.y = "Loc Number")


#import pre-model data(add coupon penetration rate calculated by Sales)
pre_model<-read.csv('pre_model.csv')
pre_model<-unique(pre_model)

#Margin Model development

#Consider heterogeneity:Random Effect

library(lme4)
reg_re2 = lmer(Margin~Sales_C_Dollar._Off_CPR+
                 Sales_C_Dollar._Off_Min_Purchase_CPR+Sales_C_Percent_Off_CPR+Sales_C_Reward_CPR+
                 Sales_C_Reward.Normal_CPR+Sales_C_Multiple_Normal_CPR+Sales_C_Percent_Off_Min_Purchase_CPR+
                 (1+Sales_C_Dollar._Off_CPR+Sales_C_Dollar._Off_Min_Purchase_CPR+Sales_C_Percent_Off_CPR+Sales_C_Reward_CPR+Sales_C_Reward.Normal_CPR+Sales_C_Multiple_Normal_CPR+Sales_C_Percent_Off_Min_Purchase_CPR|STORE_NUM),pre_model)
              
summary(reg_re2)
model<-data.frame(ranef(reg_re2)$STORE_NUM)
write.csv(model,"model_output.csv")
library(lmerTest)
anova(reg_re2)










