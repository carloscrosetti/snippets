# Data management packages
library(flexdashboard)
library(dplyr)
library(caret)
library(partykit)
library(randomForest)
library(Hmisc)
knitr::opts_chunk$set(cache=TRUE)
options(scipen = 9999)
rm(list=ls())

# Read dataset
loans <- read.csv("http://www.sci.csueastbay.edu/~esuess/classes/Statistics_6620/Presentations/ml7/credit.csv")
str(loans)

# Data management
# Change the order/level of checking_balance variable
loans$checking_balance <- factor(loans$checking_balance,
                                levels = c(" 200 DM",
                                           "unknown"))
summary(loans[loans$default == "yes", "checking_balance"])
# Change the order/level of saving_balance variable
loans$savings_balance <- factor(loans$savings_balance,
                                levels = c(" 1000 DM",
                                           "unknown"))
summary(loans[loans$default == "yes", "savings_balance"])
# Change the order/level of credit_history variable
loans$credit_history <- factor(loans$credit_history,
                                levels = c("critical",
                                           "poor",
                                           "good",
                                           "very good",
                                           "perfect"))
summary(loans[loans$default == "yes", "credit_history"])
# Change the order/level of other_credit variable
loans$other_credit <- factor(loans$other_credit,
                                levels = c("none",
                                           "store",
                                           "bank"))
summary(loans[loans$default == "yes", "other_credit"])
set.seed(300)
in_loans_train <- sample(nrow(loans), nrow(loans)*0.75)
loans_train <- loans[in_loans_train, ]
loans_test <- loans[-in_loans_train, ]

# Row {data-width=350}


### Chart A - Decision tree Model I

loans_model_dt <- ctree(default ~ ., loans_train)
plot(loans_model_dt)


### Chart B - Decision tree Model I - simple

plot(loans_model_dt, type = "simple")

# Row {data-width=650}

### Chart C - Decision tree Model Model I - formula

loans_model_dt

# Row {data-width=650}

### Chart D - Confusion Matrix for Decision tree Model Model I

loans_pred_dt <- predict(loans_model_dt, loans_test)
dt_conft <- table("prediction" = loans_pred_dt,
                   "actual" = loans_test$default)

accu_dt <- round((dt_conft[1]+dt_conft[4])/sum(dt_conft[1:4]),4)
prec_dt <- round(dt_conft[4]/(dt_conft[2]+dt_conft[4]), 4)
reca_dt <- round(dt_conft[4]/(dt_conft[4]+dt_conft[3]), 4)
spec_dt <- round(dt_conft[1]/(dt_conft[1]+dt_conft[2]), 4)
confusionMatrix(loans_pred_dt, loans_test$default, positive = "yes")

# Row {data-width=650}


### Chart E - Decision tree Model II 

loans_model_dt2 <- ctree(default ~ ., loans_train, control = ctree_control(mincriterion = 0.7))
plot(loans_model_dt2)

# Row {data-width=650}

### Chart F - Decision tree Model Model II - formula

loans_model_dt2


# Row {data-width=650}

### Chart G - Confusion Matrix for Decision tree Model Model II

loans_pred_dt2 <- predict(loans_model_dt2, loans_test)
confusionMatrix(loans_pred_dt2, loans_test$default, positive = "yes")


# Row {data-width=650}

### Chart H - Random Forest Model

set.seed(300)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, allowParallel = TRUE)

loans_rf <- train(default ~ ., data = loans, method = "rf", trControl = ctrl)

loans_rf$finalModel


# Row {data-width=650}


### Chart I - Random Forest Model - variable importance

varImp(loans_rf)


# Row {data-width=350}

### Chart J - Random Forest Model - Final model plot I

plot(loans_rf$finalModel)
legend("topright", colnames(loans_rf$finalModel$err.rate),col = 1:6, cex = 0.8, fill = 1:6)


### Chart K - Random Forest Model - Final model plot II

plot(loans_rf)