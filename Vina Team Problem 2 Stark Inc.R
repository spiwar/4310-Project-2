# Problem 2 Stark Inc. Introduction:
# After Steve Rogers replaced the Infinity Stones, Stark Enterprises has branched into the financial industry.
# Perhaps Steve Rogers changed something when he traveled back in time 2. Since Mr. Stark is on a different 
# timeline, they are short of analogical power. They would like to build a model and predict which customers
# are likely to have high risk. 

# Objectives:
# Regarding this problem, we will analyze a sample of the larger dataset 'credit_2.csv'. Based on the dataset, 
# we will build and select an appropriate model for the company and predict the risk at the target variable 
# of the new customers which was given in 'credit_test_2.csv'.

# Data Description:
# The credit_2 dataset represents ten of thousands information from customers, which includes some key variables
# like income, gender, education level, age, housing situation, family status, loan amount, and occupation. 


# Libraries ------------------------------------------------------------

library(rpart)
library(rpart.plot)
library(forecast)
library(caret)
library(ISLR)
require(tree)
library(tidyr)
library(dplyr)
library(janitor)
library(ROSE)

# Data import and transformation ---------------------------------------------
#
# For the data, we decided to remove all variables that were deemed unecessary, 
# turn all the remaining categorical variables into factors, remove missing values, 
# and change the output variable into something more human friendly (low-risk and high-risk).
#
#-----------------------------------------------------------------------------

# Data importing
credit <- read.csv("credit_2.csv", header = TRUE)
head(credit, 10)
names(credit)

# Removing all unnecessary variables
credit_filtered <- credit[, -c(1,19,20,23,24,26,32,33,34:36,41,42:61,67)]
credit_filtered <- credit_filtered [,-c(16,21,30:33)]
credit_filtered <- credit_filtered [,-c(9,10,13,18,25,27,28)]
credit_filtered <- credit_filtered [,-c(1,10:14,17)]
credit_filtered <- drop_na(credit_filtered)
names(credit_filtered)

# Changing the dependent variable into actual text
credit_filtered$TARGET <- factor(credit_filtered$TARGET,
                                 levels = c("0", "1"),
                                 labels = c("Low-risk", "High-risk"))


# Factorize all the categorical variables
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

# Training validation split & Classification Tree -------------------------------------------------------
#
# For the training of our model, we decided to use a decision tree, because it can show us
# what variables the model thinks are important when predicting a customer
# As for the actual training, we used the favorite seed number, and a 60/40 training/validation split.
# This means that 60% of the data is used to train the model, while 40% is used for validating whether
# the model is still good when used on an unfamiliar set of data.
# Since the majority of customers are low-risk, we will also need to balance the data.
#
#--------------------------------------------------------------------------------------------------------

# Setting seed & creating training & validation set 
set.seed(666)
train_index <- sample(1:nrow(credit_filtered), 0.6 * nrow(credit_filtered))
valid_index <- setdiff(1:nrow(credit_filtered), train_index)

# Inputting index to the variables
train_df <- credit_filtered[train_index, ]
valid_df <- credit_filtered[valid_index, ]

# Counting data of training and validiation
nrow(train_df)
nrow(valid_df)

compare_df_cols(train_df, valid_df)

# Balancing the data

train_df_balance <- ROSE(TARGET ~ ., data = train_df, seed = 666)$data

# Classification Tree
par(mar=c(1,1,1,1))
class_tr <- rpart(TARGET ~.,
                  data = train_df_balance, method = "class", minbucket = 2,maxdepth = 3)
prp(class_tr, cex = 0.8, tweak = 1)

# Confusion matrix

# Evaluating training set prediction
classTree_train_pred <- predict(class_tr, train_df, type = "class")
confusionMatrix(classTree_train_pred , train_df$TARGET,
                positive = "High-risk")

# Evaluating validation set prediction
classTree_valid_pred <- predict(class_tr, valid_df, type = "class")
confusionMatrix(classTree_valid_pred , valid_df$TARGET,
                positive = "High-risk")

# ROC Curve
ROSE::roc.curve(valid_df$TARGET, classTree_valid_pred)

# Evaluation ---------------------------------------------------------------------------
#
# The classification tree shows us that the model heavily relies on CNT_CHILDREN (The number of children the client has)
# and CNT_FAM_MEMBERS (How many familly members the client has).
# Accuracy is 0.6745, which means the model is 67.45% accurate at predicting in general 
# Sensitivity is 0.355, which means the model is only 35.5% accurate when predicting the positive class (high-risk)
# Specificity is 0.70194, which means the model is 70.19% accurate when predicting the negative class (low-risk)
# Results from the validation matrix is very similar to the training, so we can see that there is little to no over-fitting here.
# This means that our model will be fairly accurate across different sets of data.
# From the ROC, we can see that the AUC is 0.517, which can use some improvements
#
# -------------------------------------------------------------------------------------

# Predicting the new customers ---------------------------------------------
#
# This is where we use the new customers' information and predict whether
# they are low-risk or high-risk customers
#
#--------------------------------------------------------------------------

# Creating new customers profile
ID_250539 <- data.frame(NAME_CONTRACT_TYPE= "Cash loans",
                        CODE_GENDER = "F",FLAG_OWN_CAR="Y",FLAG_OWN_REALTY="Y",
                        CNT_CHILDREN = 0,AMT_INCOME_TOTAL = 112500, AMT_GOODS_PRICE = 202500,
                        FLAG_WORK_PHONE = 1, FLAG_EMAIL = 0,CNT_FAM_MEMBERS = 1, REGION_RATING_CLIENT =2,
                        LIVE_REGION_NOT_WORK_REGION = 1, REG_CITY_NOT_WORK_CITY =1, 
                        AMT_REQ_CREDIT_BUREAU_YEAR = 3)

ID_442361 <- data.frame(NAME_CONTRACT_TYPE= "Cash loans",
                        CODE_GENDER = "F",FLAG_OWN_CAR="Y",FLAG_OWN_REALTY="N",
                        CNT_CHILDREN = 0,AMT_INCOME_TOTAL = 135000, AMT_GOODS_PRICE = 454500,
                        FLAG_WORK_PHONE = 1, FLAG_EMAIL = 0,CNT_FAM_MEMBERS = 2, REGION_RATING_CLIENT =2,
                        LIVE_REGION_NOT_WORK_REGION = 0, REG_CITY_NOT_WORK_CITY =0, 
                        AMT_REQ_CREDIT_BUREAU_YEAR = 2)
ID_280650 <- data.frame(NAME_CONTRACT_TYPE= "Cash loans",
                        CODE_GENDER = "F",FLAG_OWN_CAR="Y",FLAG_OWN_REALTY="N",
                        CNT_CHILDREN = 0,AMT_INCOME_TOTAL = 112500, AMT_GOODS_PRICE = 450000,
                        FLAG_WORK_PHONE = 0, FLAG_EMAIL = 0,CNT_FAM_MEMBERS = 3, REGION_RATING_CLIENT =3,
                        LIVE_REGION_NOT_WORK_REGION = 0, REG_CITY_NOT_WORK_CITY =0, 
                        AMT_REQ_CREDIT_BUREAU_YEAR = 5)
ID_420851 <- data.frame(NAME_CONTRACT_TYPE= "Cash loans",
                        CODE_GENDER = "M",FLAG_OWN_CAR="Y",FLAG_OWN_REALTY="Y",
                        CNT_CHILDREN = 1,AMT_INCOME_TOTAL = 180000, AMT_GOODS_PRICE = 283500,
                        FLAG_WORK_PHONE = 0, FLAG_EMAIL = 0,CNT_FAM_MEMBERS = 3, REGION_RATING_CLIENT =2,
                        LIVE_REGION_NOT_WORK_REGION = 0, REG_CITY_NOT_WORK_CITY =0, 
                        AMT_REQ_CREDIT_BUREAU_YEAR = 2)
ID_405632 <- data.frame(NAME_CONTRACT_TYPE= "Cash loans",
                        CODE_GENDER = "M",FLAG_OWN_CAR="Y",FLAG_OWN_REALTY="Y",
                        CNT_CHILDREN = 0,AMT_INCOME_TOTAL = 247500, AMT_GOODS_PRICE = 531000,
                        FLAG_WORK_PHONE = 0, FLAG_EMAIL = 0,CNT_FAM_MEMBERS = 2, REGION_RATING_CLIENT =2,
                        LIVE_REGION_NOT_WORK_REGION = 0, REG_CITY_NOT_WORK_CITY =1, 
                        AMT_REQ_CREDIT_BUREAU_YEAR = 3)

ID_405632 <- data.frame(NAME_CONTRACT_TYPE= "Cash loans",
                        CODE_GENDER = "M",FLAG_OWN_CAR="Y",FLAG_OWN_REALTY="Y",
                        CNT_CHILDREN = 0,AMT_INCOME_TOTAL = 247500, AMT_GOODS_PRICE = 531000,
                        FLAG_WORK_PHONE = 0, FLAG_EMAIL = 0,CNT_FAM_MEMBERS = 2, REGION_RATING_CLIENT =2,
                        LIVE_REGION_NOT_WORK_REGION = 0, REG_CITY_NOT_WORK_CITY =1, 
                        AMT_REQ_CREDIT_BUREAU_YEAR = 3)

## Converting all variables into factor

ID_250539 $NAME_CONTRACT_TYPE  <- as.factor(ID_250539$NAME_CONTRACT_TYPE)
ID_250539 $CODE_GENDER <- as.factor(ID_250539 $CODE_GENDER)
ID_250539 $FLAG_OWN_CAR <- as.factor(ID_250539 $FLAG_OWN_CAR)
ID_250539 $FLAG_OWN_REALTY <- as.factor(ID_250539 $FLAG_OWN_REALTY)
ID_250539 $FLAG_WORK_PHONE <- as.factor(ID_250539 $FLAG_WORK_PHONE)
ID_250539 $FLAG_EMAIL <- as.factor(ID_250539 $FLAG_EMAIL)
ID_250539 $REGION_RATING_CLIENT <- as.factor(ID_250539 $REGION_RATING_CLIENT)
ID_250539 $LIVE_REGION_NOT_WORK_REGION <- as.factor(ID_250539 $LIVE_REGION_NOT_WORK_REGION)
ID_250539 $REG_CITY_NOT_WORK_CITY <- as.factor(ID_250539 $REG_CITY_NOT_WORK_CITY)
str(ID_250539)

ID_442361 $NAME_CONTRACT_TYPE  <- as.factor(ID_442361$NAME_CONTRACT_TYPE)
ID_442361 $CODE_GENDER <- as.factor(ID_442361 $CODE_GENDER)
ID_442361 $FLAG_OWN_CAR <- as.factor(ID_442361 $FLAG_OWN_CAR)
ID_442361 $FLAG_OWN_REALTY <- as.factor(ID_442361 $FLAG_OWN_REALTY)
ID_442361 $FLAG_WORK_PHONE <- as.factor(ID_442361 $FLAG_WORK_PHONE)
ID_442361 $FLAG_EMAIL <- as.factor(ID_442361 $FLAG_EMAIL)
ID_442361 $REGION_RATING_CLIENT <- as.factor(ID_442361 $REGION_RATING_CLIENT)
ID_442361 $LIVE_REGION_NOT_WORK_REGION <- as.factor(ID_442361 $LIVE_REGION_NOT_WORK_REGION)
ID_442361 $REG_CITY_NOT_WORK_CITY <- as.factor(ID_442361 $REG_CITY_NOT_WORK_CITY)

ID_280650 $NAME_CONTRACT_TYPE  <- as.factor(ID_280650$NAME_CONTRACT_TYPE)
ID_280650 $CODE_GENDER <- as.factor(ID_280650 $CODE_GENDER)
ID_280650 $FLAG_OWN_CAR <- as.factor(ID_280650 $FLAG_OWN_CAR)
ID_280650 $FLAG_OWN_REALTY <- as.factor(ID_280650 $FLAG_OWN_REALTY)
ID_280650 $FLAG_WORK_PHONE <- as.factor(ID_280650 $FLAG_WORK_PHONE)
ID_280650 $FLAG_EMAIL <- as.factor(ID_280650 $FLAG_EMAIL)
ID_280650 $REGION_RATING_CLIENT <- as.factor(ID_280650 $REGION_RATING_CLIENT)
ID_280650 $LIVE_REGION_NOT_WORK_REGION <- as.factor(ID_280650 $LIVE_REGION_NOT_WORK_REGION)
ID_280650 $REG_CITY_NOT_WORK_CITY <- as.factor(ID_280650 $REG_CITY_NOT_WORK_CITY)

ID_420851 $NAME_CONTRACT_TYPE  <- as.factor(ID_420851$NAME_CONTRACT_TYPE)
ID_420851 $CODE_GENDER <- as.factor(ID_420851 $CODE_GENDER)
ID_420851 $FLAG_OWN_CAR <- as.factor(ID_420851 $FLAG_OWN_CAR)
ID_420851 $FLAG_OWN_REALTY <- as.factor(ID_420851 $FLAG_OWN_REALTY)
ID_420851 $FLAG_WORK_PHONE <- as.factor(ID_420851 $FLAG_WORK_PHONE)
ID_420851 $FLAG_EMAIL <- as.factor(ID_420851 $FLAG_EMAIL)
ID_420851 $REGION_RATING_CLIENT <- as.factor(ID_420851 $REGION_RATING_CLIENT)
ID_420851 $LIVE_REGION_NOT_WORK_REGION <- as.factor(ID_420851 $LIVE_REGION_NOT_WORK_REGION)
ID_420851 $REG_CITY_NOT_WORK_CITY <- as.factor(ID_420851 $REG_CITY_NOT_WORK_CITY)
str(ID_420851)

ID_405632$NAME_CONTRACT_TYPE  <- as.factor(ID_405632$NAME_CONTRACT_TYPE)
ID_405632 $CODE_GENDER <- as.factor(ID_405632 $CODE_GENDER)
ID_405632 $FLAG_OWN_CAR <- as.factor(ID_405632 $FLAG_OWN_CAR)
ID_405632 $FLAG_OWN_REALTY <- as.factor(ID_405632 $FLAG_OWN_REALTY)
ID_405632 $FLAG_WORK_PHONE <- as.factor(ID_405632 $FLAG_WORK_PHONE)
ID_405632 $FLAG_EMAIL <- as.factor(ID_405632 $FLAG_EMAIL)
ID_405632 $REGION_RATING_CLIENT <- as.factor(ID_405632 $REGION_RATING_CLIENT)
ID_405632 $LIVE_REGION_NOT_WORK_REGION <- as.factor(ID_405632 $LIVE_REGION_NOT_WORK_REGION)
ID_405632 $REG_CITY_NOT_WORK_CITY <- as.factor(ID_405632 $REG_CITY_NOT_WORK_CITY)
str(ID_405632)

# Predicting the new customers

ID_250539_pred <- predict(class_tr, newdata = ID_250539)
ID_250539_pred

ID_442361_pred <- predict(class_tr, newdata = ID_442361)
ID_442361_pred

ID_280650_pred <- predict(class_tr, newdata = ID_280650)
ID_280650_pred

ID_420851_pred <- predict(class_tr, newdata = ID_420851)
ID_420851_pred

ID_405632_pred <- predict(class_tr, newdata = ID_405632)
ID_405632_pred

# The model predicted 3 customers as low-risk, and 2 customers as high-risk

# Discussion & Final Recommendation ------------------------------------------------------------
#
# Our final model was fairly accurate, as both the training and validation accuracy was fairly decent (67%)
# However, The sensitivity, as well as the ROC curve has room for improvements. The model's accuracy rate could be improved by
# choosing variables that are more correlated to the outcome variable (TARGET).
# Overall, we feel that the model is decent and can be used in real-world situations.
#
#---------------------------------------------------------------------------------------------


