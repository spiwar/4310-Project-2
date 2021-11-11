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
library(dplyr)
credit <- drop_na(credit)
head(credit, 10)
names(credit)

#remove unnecessary vars
credit_filtered <- credit[, -c(1,19,20,23,24,26,32,33,34:36,41,42:61)]
credit_filtered <- credit_filtered [,-c(16,21,30:33)]
credit_filtered <- credit_filtered [,-c(9,10,13,18,25,27,28)]
names(credit_filtered)
str(credit_filtered)
credit_filtered_cleaned <- na.omit(credit_filtered) 
#Convert into binary
levels(credit_filtered_cleaned$NAME_TYPE_SUITE) <- 
  list(Unaccompanied = "Single",
       other = "Group")
levels(credit_filtered_cleaned$NAME_TYPE_SUITE)

levels(credit_filtered_cleaned$NAME_FAMILY_STATUS) <- 
  list ("Married","Civil marriage" = "In marriage",
        other = "Non-married")
levels(credit_filtered_cleaned$NAME_HOUSING_TYPE) <- 
  list ("With parents" = "Live w/ parents",
        other = "No parents")
levels(credit_filtered_cleaned$OCCUPATION_TYPE) <- 
  list ("Cleaning staff","Cooking staff","Core staff","Drivers","Laborers","Low-skill Laborers","Waiters/Barmen staff" = "Blue Collar",
        other = "White Collar")

str(credit_filtered_cleaned)
#factorise all the categorical vars
credit_filtered_cleaned $TARGET <- as.factor(credit_filtered_cleaned $TARGET)
credit_filtered_cleaned $NAME_CONTRACT_TYPE  <- as.factor(credit_filtered_cleaned $NAME_CONTRACT_TYPE)
credit_filtered_cleaned $CODE_GENDER <- as.factor(credit_filtered_cleaned $CODE_GENDER)
#credit_filtered_cleaned $FLAG_OWN_CAR <- as.factor(credit_filtered_cleaned $FLAG_OWN_CAR)
credit_filtered_cleaned $FLAG_OWN_REALTY <- as.factor(credit_filtered_cleaned $FLAG_OWN_REALTY)
credit_filtered_cleaned $NAME_HOUSING_TYPE <- as.factor(credit_filtered_cleaned $NAME_HOUSING_TYPE)
credit_filtered_cleaned $OCCUPATION_TYPE <- as.factor(credit_filtered_cleaned $OCCUPATION_TYPE)
credit_filtered_cleaned $NAME_TYPE_SUITE<- as.factor(credit_filtered_cleaned $NAME_TYPE_SUITE)
credit_filtered_cleaned $NAME_FAMILY_STATUS <- as.factor(credit_filtered_cleaned$NAME_FAMILY_STATUS)
str(credit_filtered_cleaned)