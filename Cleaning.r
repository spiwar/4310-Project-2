library(rpart)
library(rpart.plot)
#install.packages("fracdiff")
library(forecast)
library(caret)
library(ISLR)
data(package="ISLR")
require(tree)
library(tidyr)
library(dplyr)

## ----- Load data
credit <- read.csv("credit_2.csv", header = TRUE)
credit <- drop_na(credit)
head(credit, 10)
names(credit)

#remove unnecessary vars
credit_filtered <- credit[, -c(1,19,20,23,24,26,32,33,34:36,41,42:61)]
credit_filtered <- credit_filtered [,-c(16,21,30:33)]
credit_filtered <- credit_filtered [,-c(9,10,13,18,25,27,28)]
credit_filtered <- credit_filtered [,-c(1,10:14,17)]
names(credit_filtered)

#change the dependent var into actual text
credit_filtered$TARGET <- factor(credit_filtered$TARGET,
                                 levels = c("0", "1"),
                                 labels = c("Low-risk", "High-risk"))
#factorise all the categorical vars
credit_filtered $TARGET <- as.factor(credit_filtered $TARGET)
credit_filtered $NAME_CONTRACT_TYPE  <- as.factor(credit_filtered $NAME_CONTRACT_TYPE)
credit_filtered $CODE_GENDER <- as.factor(credit_filtered $CODE_GENDER)
credit_filtered $FLAG_OWN_CAR <- as.factor(credit_filtered $FLAG_OWN_CAR)
credit_filtered $FLAG_OWN_REALTY <- as.factor(credit_filtered $FLAG_OWN_REALTY)
credit_filtered $FLAG_WORK_PHONE <- as.factor(credit_filtered $FLAG_WORK_PHONE)
credit_filtered $FLAG_EMAIL <- as.factor(credit_filtered $FLAG_EMAIL)
credit_filtered $REGION_RATING_CLIENT <- as.factor(credit_filtered $REGION_RATING_CLIENT)
credit_filtered $LIVE_REGION_NOT_WORK_REGION <- as.factor(credit_filtered $LIVE_REGION_NOT_WORK_REGION)
credit_filtered $REG_CITY_NOT_WORK_CITY <- as.factor(credit_filtered $REG_CITY_NOT_WORK_CITY)


str(credit_filtered)
names(credit_filtered)

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
train_df_balance <- ROSE(TARGET ~ ., data = train_df, seed = 666)$credit_filtered

par(mar=c(1,1,1,1))
class_tr <- rpart(TARGET ~.,
                  data = train_df, method = "class", minbucket = 5,maxdepth = 10)
prp(class_tr, cex = 0.8, tweak = 1)



