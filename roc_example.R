setwd("C:/Users/proud/Documents/UT/Teaching/2017_Fall_SDS_383_Statistical_Modeling/r_demos")

###
### ROC Example.
###

###
### Get the libraries needed:
###

library(DAAG)
library(MASS)
library(e1071)
library(pROC)

###
### Table to hold values:
###

summary_table = matrix(NA, 3, 2)
colnames(summary_table) = c("Train AUC", " Test AUC")
rownames(summary_table) = c("Logit","LDA","QDA")
summary_table

###
### Again, split the data into train and test:
###

set.seed(321)
ind = sample(1:3, size = nrow(spam7), replace = TRUE)

test_index =  which(ind == 1)
train      =  spam7[ -test_index, ] 
test       =  spam7[  test_index, ] 

###
### Create the three models and get probabilities:
###
### Ignore the warning.
###
### Getting the probababilities from LDA and QDA is
###  not intuitive.
###

logit_model = glm(yesno ~ ., data = train, family = binomial)
logit_pred  = logit_model$fitted.values

lda_model   = lda(yesno ~ . , data = train)
lda_pred    = predict(lda_model)$posterior[,"y"]

qda_model   = qda(yesno ~ . , data = train)
qda_pred    = predict(qda_model)$posterior[,"y"]

###
### Look at the training AUC's:
###

par(mfrow = c(1,2))

roc_logit = roc(response = train$yesno, predictor = logit_pred, plot=T, 
                main = "Train AUC", col = "green")
roc_logit$auc
summary_table[1,1] = roc_logit$auc

roc_lda  = roc(response = train$yesno, predictor = lda_pred, plot=T, add = T, col = "black")
roc_lda$auc
summary_table[2,1] = roc_lda$auc

roc_qda = roc(response =train$yesno, predictor = qda_pred, plot=T, add = T, col = "red")
roc_qda$auc
summary_table[3,1] = roc_lda$auc

legend("bottomright", c("Logistic","LDA","QDA"), lwd = "2", 
        col = c("green","black","red"), bty = "n")

summary_table

###
### Let's predict out of sample:
###

logit_pred_out = predict(logit_model, newdata = test, type = "response")
lda_pred_out   = predict(lda_model,   newdata = test)$posterior[,"y"]
qda_pred_out   = predict(qda_model,   newdata = test)$posterior[,"y"]

###
### Now the out of sample AUC:
###

summary_table[1,2] = roc(response = test$yesno, predictor = logit_pred_out, 
                         plot=T, main = "Test AUC", col = "green")$auc

summary_table[2,2] = roc(response = test$yesno, predictor = lda_pred_out,   
                         plot=T, add = T, col = "black")$auc

summary_table[3,2] = roc(response = test$yesno, predictor = qda_pred_out,   
                        plot=T, add = T, col = "red")$auc

legend("bottomright", c("Logistic","LDA","QDA"), lwd = "2", 
        col = c("green","black","red"), bty = "n")

###
### Let's look at the summarized values:
###

round(summary_table,3)


