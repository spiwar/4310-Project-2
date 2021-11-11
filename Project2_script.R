library(rpart)
library(rpart.plot)
#install.packages("fracdiff")
library(forecast)
library(caret)
library(ISLR)
data(package="ISLR")
require(tree)


## ----- Load data
credit <- read.csv("credit_2_cleaned.csv", header = TRUE)
library(tidyr)
credit <- drop_na(credit)
head(credit, 10)
names(credit)

#remove unnecessary vars
credit_filtered <- credit[, -c(19,20,23,24,26,32,33,34:36,41,42:61)]

names(credit_filtered)
str(credit_filtered)

#factorise all the categorical vars
credit_filtered$NAME_CONTRACT_TYPE <- as.factor(credit_filtered$NAME_CONTRACT_TYPE)
credit_filtered$CODE_GENDER <- as.factor(credit_filtered$CODE_GENDER)
credit_filtered$FLAG_OWN_CAR <- as.factor(credit_filtered$FLAG_OWN_CAR)
credit_filtered$FLAG_OWN_REALTY <- as.factor(credit_filtered$FLAG_OWN_REALTY)
credit_filtered$NAME_TYPE_SUITE <- as.factor(credit_filtered$NAME_TYPE_SUITE)
credit_filtered$NAME_EDUCATION_TYPE <- as.factor(credit_filtered$NAME_EDUCATION_TYPE)
credit_filtered$NAME_FAMILY_STATUS <- as.factor(credit_filtered$NAME_FAMILY_STATUS)
credit_filtered$NAME_HOUSING_TYPE <- as.factor(credit_filtered$NAME_HOUSING_TYPE)
credit_filtered$NAME_INCOME_TYPE <- as.factor(credit_filtered$NAME_INCOME_TYPE)
credit_filtered$OCCUPATION_TYPE <- as.factor(credit_filtered$OCCUPATION_TYPE)
credit_filtered$ORGANIZATION_TYPE <- as.factor(credit_filtered$ORGANIZATION_TYPE)

#look at new order
t(t(names(credit_filtered)))
str(credit_filtered)
nrow(credit_filtered)

#change the dependent var into actual text
credit_filtered$TARGET <- factor(credit_filtered$TARGET,
                                 levels = c("0", "1"),
                                 labels = c("Low-risk", "High-risk"))

### training validation set
set.seed(666)
train_index <- sample(1:nrow(credit_filtered), 0.6 * nrow(credit_filtered))
valid_index <- setdiff(1:nrow(credit_filtered), train_index)

train_df <- credit_filtered[train_index, ]
valid_df <- credit_filtered[valid_index, ]

nrow(train_df)
nrow(valid_df)

library(janitor)
compare_df_cols(train_df, valid_df)

library(ROSE)

train_df_balance <- ROSE(TARGET ~ SK_ID_CURR + NAME_CONTRACT_TYPE + CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY
                         + CNT_CHILDREN + AMT_INCOME_TOTAL + AMT_CREDIT + AMT_ANNUITY
                         + AMT_GOODS_PRICE + NAME_TYPE_SUITE + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE +
                           NAME_FAMILY_STATUS + NAME_HOUSING_TYPE + DAYS_BIRTH + DAYS_EMPLOYED +
                           OWN_CAR_AGE + FLAG_MOBIL + FLAG_CONT_MOBILE + FLAG_EMAIL + OCCUPATION_TYPE +
                           CNT_FAM_MEMBERS + REGION_RATING_CLIENT + REGION_RATING_CLIENT_W_CITY +
                           REG_CITY_NOT_LIVE_CITY + REG_CITY_NOT_WORK_CITY + LIVE_CITY_NOT_WORK_CITY + 
                           ORGANIZATION_TYPE + AMT_REQ_CREDIT_BUREAU_HOUR + AMT_REQ_CREDIT_BUREAU_DAY + 
                           AMT_REQ_CREDIT_BUREAU_WEEK + AMT_REQ_CREDIT_BUREAU_MON + AMT_REQ_CREDIT_BUREAU_QRT +
                           AMT_REQ_CREDIT_BUREAU_YEAR, data = train_df, seed = 666)$credit_filtered


#par(mar=c(1,1,1,1))
class_tr <- rpart(TARGET ~ SK_ID_CURR + NAME_CONTRACT_TYPE + CODE_GENDER + FLAG_OWN_CAR + FLAG_OWN_REALTY
                  + CNT_CHILDREN + AMT_INCOME_TOTAL + AMT_CREDIT + AMT_ANNUITY
                  + AMT_GOODS_PRICE + NAME_TYPE_SUITE + NAME_INCOME_TYPE + NAME_EDUCATION_TYPE +
                    NAME_FAMILY_STATUS + NAME_HOUSING_TYPE + DAYS_BIRTH + DAYS_EMPLOYED +
                    OWN_CAR_AGE + FLAG_MOBIL + FLAG_CONT_MOBILE + FLAG_EMAIL + OCCUPATION_TYPE +
                    CNT_FAM_MEMBERS + REGION_RATING_CLIENT + REGION_RATING_CLIENT_W_CITY +
                    REG_CITY_NOT_LIVE_CITY + REG_CITY_NOT_WORK_CITY + LIVE_CITY_NOT_WORK_CITY + 
                    ORGANIZATION_TYPE + AMT_REQ_CREDIT_BUREAU_HOUR + AMT_REQ_CREDIT_BUREAU_DAY + 
                    AMT_REQ_CREDIT_BUREAU_WEEK + AMT_REQ_CREDIT_BUREAU_MON + AMT_REQ_CREDIT_BUREAU_QRT +
                    AMT_REQ_CREDIT_BUREAU_YEAR,
                  data = train_df_balance, method = "class", minbucket = 5,maxdepth = 10)
prp(class_tr, cex = 0.8, tweak = 1)
