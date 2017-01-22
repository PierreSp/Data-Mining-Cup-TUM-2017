# Business Analytics
# Data Mining Cup Introduction
#
# Please note, that this script only has the nature of proposal. It provides useful functions for the steps of data mining but does not cover all possibilities.

# The caret package is used (http://topepo.github.io/caret/index.html)
# install.packages("caret")
# install.packages("lubridate")
# install.packages("arules")
# install.packages("FSelector")
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

library(FSelector)
library(arules)
library(caret)
library(lubridate)
library(data.table)
library(dplyr)
library(magrittr)
#clear environment variables
rm(list=ls())

# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed
start_time = Sys.time()


##############
# Functions
##############


######################################################
# 1. Build a Team in the DMC Manager
# https://dmc.dss.in.tum.de/dmc/
# Login with TUM login data ("TUM-Kennung")
#
# Found or join a team (size: 1-4 students)


######################################################
# 2. Load & Explore the Training Data Set
# Import data
training_data <- read.csv("DMC2/2_ba16_dmc2_vehicle_training_data_Ar2kCbl.csv")
test_data <- read.csv("DMC2/pub_nkAt59S.csv")

# Remove ID field
training_data = training_data[, -1]
training_data = training_data[, -1]

######################################################
# 3. Data Preparation
# (using both training and test data)
# do NOT DELETE any instances in the test data

# Nominal attributes

training_data$engine_type = factor(training_data$engine_type)
test_data$engine_type = factor(test_data$engine_type)

training_data$vehicle_type = factor(training_data$vehicle_type)
test_data$vehicle_type = factor(test_data$vehicle_type)
# Binning/Discretization

# column creation
training_data$engine_feature_14mean <- (
  training_data$engine_feature_14_1 * 1 +
    training_data$engine_feature_14_2 * 2 +
    training_data$engine_feature_14_3 * 3 +
    training_data$engine_feature_14_4 * 4 +
    training_data$engine_feature_14_5 * 5) / 100
training_data <- training_data[, -(training_data %>% colnames() %>% grep(pattern='engine_feature_14_'))]

test_data$engine_feature_14mean <- (
  test_data$engine_feature_14_1 * 1 +
    test_data$engine_feature_14_2 * 2 +
    test_data$engine_feature_14_3 * 3 +
    test_data$engine_feature_14_4 * 4 +
    test_data$engine_feature_14_5 * 5) / 100


training_data$engine_feature_11mean <- (
  training_data$engine_feature_11_1 * 1 +
    training_data$engine_feature_11_2 * 2 +
    training_data$engine_feature_11_3 * 3 +
    training_data$engine_feature_11_4 * 4 +
    training_data$engine_feature_11_5 * 5 +
    training_data$engine_feature_11_6 * 6 +
    training_data$engine_feature_11_7 * 7 +
    training_data$engine_feature_11_8 * 8 +
    training_data$engine_feature_11_9 * 9 +
    training_data$engine_feature_11_10 * 10 +
    training_data$engine_feature_11_11 * 11 +
    training_data$engine_feature_11_12 * 12 +
    training_data$engine_feature_11_13 * 13 +
    training_data$engine_feature_11_14 * 14 +
    training_data$engine_feature_11_15 * 15 +
    training_data$engine_feature_11_16 * 16 +
    training_data$engine_feature_11_17 * 17 +
    training_data$engine_feature_11_18 * 18 +
    training_data$engine_feature_11_19 * 19 +
    training_data$engine_feature_11_20 * 20 +
    training_data$engine_feature_11_21 * 21 +
    training_data$engine_feature_11_22 * 22 +
    training_data$engine_feature_11_23 * 23 +
    training_data$engine_feature_11_24 * 24 +
    training_data$engine_feature_11_25 * 25 +
    training_data$engine_feature_11_26 * 26 +
    training_data$engine_feature_11_27 * 27 +
    training_data$engine_feature_11_28 * 28 +
    training_data$engine_feature_11_29 * 29 +
    training_data$engine_feature_11_30 * 30 +
    training_data$engine_feature_11_31 * 31 +
    training_data$engine_feature_11_32 * 32 +
    training_data$engine_feature_11_33 * 33 +
    training_data$engine_feature_11_34 * 34 +
    training_data$engine_feature_11_35 * 35 +
    training_data$engine_feature_11_36 * 36 +
    training_data$engine_feature_11_37 * 37 +
    training_data$engine_feature_11_38 * 38 +
    training_data$engine_feature_11_39 * 39 +
    training_data$engine_feature_11_40 * 40 +
    training_data$engine_feature_11_41 * 41 +
    training_data$engine_feature_11_42 * 42 +
    training_data$engine_feature_11_43 * 43 +
    training_data$engine_feature_11_44 * 44 +
    training_data$engine_feature_11_45 * 45 +
    training_data$engine_feature_11_46 * 46 +
    training_data$engine_feature_11_47 * 47 +
    training_data$engine_feature_11_48 * 48) / 100
#training_data <- training_data[, -(training_data %>% colnames() %>% grep(pattern='engine_feature_11_'))]



test_data$engine_feature_11mean <- (
  test_data$engine_feature_11_1 * 1 +
    test_data$engine_feature_11_2 * 2 +
    test_data$engine_feature_11_3 * 3 +
    test_data$engine_feature_11_4 * 4 +
    test_data$engine_feature_11_5 * 5 +
    test_data$engine_feature_11_6 * 6 +
    test_data$engine_feature_11_7 * 7 +
    test_data$engine_feature_11_8 * 8 +
    test_data$engine_feature_11_9 * 9 +
    test_data$engine_feature_11_10 * 10 +
    test_data$engine_feature_11_11 * 11 +
    test_data$engine_feature_11_12 * 12 +
    test_data$engine_feature_11_13 * 13 +
    test_data$engine_feature_11_14 * 14 +
    test_data$engine_feature_11_15 * 15 +
    test_data$engine_feature_11_16 * 16 +
    test_data$engine_feature_11_17 * 17 +
    test_data$engine_feature_11_18 * 18 +
    test_data$engine_feature_11_19 * 19 +
    test_data$engine_feature_11_20 * 20 +
    test_data$engine_feature_11_21 * 21 +
    test_data$engine_feature_11_22 * 22 +
    test_data$engine_feature_11_23 * 23 +
    test_data$engine_feature_11_24 * 24 +
    test_data$engine_feature_11_25 * 25 +
    test_data$engine_feature_11_26 * 26 +
    test_data$engine_feature_11_27 * 27 +
    test_data$engine_feature_11_28 * 28 +
    test_data$engine_feature_11_29 * 29 +
    test_data$engine_feature_11_30 * 30 +
    test_data$engine_feature_11_31 * 31 +
    test_data$engine_feature_11_32 * 32 +
    test_data$engine_feature_11_33 * 33 +
    test_data$engine_feature_11_34 * 34 +
    test_data$engine_feature_11_35 * 35 +
    test_data$engine_feature_11_36 * 36 +
    test_data$engine_feature_11_37 * 37 +
    test_data$engine_feature_11_38 * 38 +
    test_data$engine_feature_11_39 * 39 +
    test_data$engine_feature_11_40 * 40 +
    test_data$engine_feature_11_41 * 41 +
    test_data$engine_feature_11_42 * 42 +
    test_data$engine_feature_11_43 * 43 +
    test_data$engine_feature_11_44 * 44 +
    test_data$engine_feature_11_45 * 45 +
    test_data$engine_feature_11_46 * 46 +
    test_data$engine_feature_11_47 * 47 +
    test_data$engine_feature_11_48 * 48) / 100

# training_data = training_data[-c(16:63)]

# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(defect ~ .^2 , data=training_data)
weights_info_gain
weights_gain_ratio = gain.ratio(defect ~ .^2 , data=training_data)
weights_gain_ratio

# Select the most important attributes based on Gain Ratio
most_important_attributes <- cutoff.k(weights_gain_ratio, 70)
most_important_attributes
formula_with_most_important_attributes <- as.simple.formula(most_important_attributes, "defect")
formula_with_most_important_attributes

#Create formula manually
eng_feature_cols <- colnames(training_data)[training_data %>% colnames() %>% grepl(pattern='engine_feature')]
other_cols <- colnames(training_data[, -(ncol(training_data)-2)])[!(training_data[, -(ncol(training_data)-2)] %>% colnames() %>% grepl(pattern='engine_feature'))]
my_formula = paste(
  paste(
    unlist(
      lapply(
        X = eng_feature_cols,
        FUN = function(x) paste(other_cols, x, sep = "*", collapse = " + "))),
    collapse = " + "),
  paste(
    colnames(training_data), collapse = " + "),
  sep=" + ")
formula_with_most_important_attributes <- as.simple.formula(my_formula, "defect")


######################################################
# 4. Training & Evaluation
# 10 x 10-fold cross validation


cv.ctrl <- trainControl(method = "repeatedcv", repeats = 4, number = 5, 
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds =  c(500,1000),
                        eta = c(0.01, 0.001),
                        max_depth = c(4,8),
                        subsample = c(0.5,0.75,1),
                        min_child_weight= 0.2,
                        gamma =  0,
                        colsample_bytree = c(0.4,0.6,0.8)
)

# xgb.grid <- expand.grid(nrounds =  1000,
#                         eta = 0.01,
#                         max_depth = 4,
#                         subsample = 1,
#                         min_child_weight= 0.2,
#                         gamma =  0,
#                         colsample_bytree = 0.4
# )
xgb_tune <-train(formula_with_most_important_attributes,
                 data=training_data,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose=T,
                 metric="Accuracy",
                 nthread = 26,
                 na.action = na.pass
)

print(xgb_tune$finalModel)
print(max(xgb_tune$results[8]))

######################################################
# 5. Predict Classes in Test Data
prediction_classes = predict.train(object=xgb_tune, newdata=test_data, na.action=na.pass)
predictions = data.frame(id=test_data$system_readout_id, prediction=prediction_classes)
# predictions

######################################################
# 6. Export the Predictions
write.csv(predictions, file="prediction_dmc2_dataRtists_5.csv", row.names=FALSE)
print(Sys.time() - start_time)

