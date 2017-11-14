setwd("\\SDS\\HW6")
train = read.csv("train.csv", header = F, nrows = 1600)
test = read.csv("test.csv", header = F,nrows =400)
train$V785 = as.factor(train$V785)
test$V785 = as.factor(test$V785)

get_error = function(predicted, observed)
{
  x = table(predicted, observed)
  print(x)
  cat("\n")
  cat( 100 * round( 1-sum(diag(x))/sum(x), 4), sep = "", "%\n\n")
}

###############################################################################################
##
##preprocessing with PCA
##
library(caret)
library(vegan)
PCA_Var_number=100                                        ##the components after select
pca_pre=function(train){
  dist_matrix <- vegdist(train[-785],method = "euclidean", binary = FALSE,
                         diag = FALSE,upper = FALSE,na.rm = TRUE)
  PCA_vars <- cmdscale(d = dist_matrix,eig = TRUE, add = FALSE, x.ret = FALSE,
                       k = PCA_Var_number)                       
  return(PCA_vars)
}

transfer_Data=function(PCA_vars,train){
  a <- PCA_vars$points
  a <- cbind(a,train$V785)
  a<-as.data.frame(a)
  return(a)
}

train_tep<-pca_pre(train)
train_pca <- transfer_Data(train_tep,train)
test_tep<-pca_pre(test)
test_pca <- transfer_Data(test_tep,test)

###############################################################################################
##
##Model 1. LDA
##
lda.
library (MASS)
?lda
lda.fit<-lda(V400 ~ ., data = train_pca,tol = 1.0e-7 ,method="mle" )

lda.fit<-lda(grouping=train$V785, x=train[-785])#,  tol = 1.0e-4, method="mle", CV = FALSE )

lda.train.predict <- predict(lda.fit)$class
get_error(lda.train.predict , train$V785)

lda.test.predict <- predict(lda.fit, newdata = test_pca)$class
get_error(lda.test.predict , test$V785)

###############################################################################################
##
##Model 2. QDA
##

qda_model = qda(V400 ~ . , data = train_pca)
qda_train_predictions = predict(qda_model)$class
get_error( qda_train_predictions , train$V785 )

qda_test_predictions = predict(qda_model, newdata = test_pca)$class
get_error( qda_test_predictions , test$V785 )

###############################################################################################
##
##Model 3. Na??ve Bayes
##

nb_model = naiveBayes(V785 ~ ., data = train)

nb_predictions = predict(nb_model, newdata = train)
get_error( nb_predictions , train$V785 )

nb_predictions = predict(nb_model, newdata = test)
get_error( nb_predictions , test$V785 )

###############################################################################################
##
##Model 4. SVM
##
?svm
svm_model <- svm(train[-785], train$V785, type='C', kernel='polynomial')##best among linear,polynomial,radial basis, sigmoid

svm_train_prediction <- predict(svm_model, train[-785])
get_error( svm_train_prediction , train$V785 )

svm_test_prediction <- predict(svm_model, test[-785])
get_error( svm_test_prediction , test$V785 )

###############################################################################################
## Model 5. Single Tree
##
library(rpart)
?rpart
tree_model = rpart(V785 ~ ., data = train, method="class",control = rpart.control(cp = 0.001))
plot(tree_model, uniform=T,branch=0.2,margin=0.1)
text(tree_model)

tree_train_predict <- predict(tree_model, newdata = train)
train_best<-colnames(tree_train_predict)[max.col(tree_train_predict,ties.method="first")]
get_error( train_best , train$V785 )

tree_test_predict <- predict(tree_model, newdata = test)
test_best<-colnames(tree_test_predict)[max.col(tree_test_predict,ties.method="first")]
get_error( test_best , test$V785 )

###############################################################################################
## Model 6. Random Forest
##

library(ranger)
?ranger
rf_fit = ranger(V785 ~ ., data = train, importance = "impurity")

rf_train_prediction = predict(rf_fit, data = train)
get_error(rf_train_prediction$predictions,train$V785)

rf_test_prediction = predict(rf_fit, data = test)
get_error(rf_test_prediction$predictions,test$V785)

###############################################################################################
## Model 7. Gradient Boosting
##

library(gbm)
###
### Build GBM Regression Model.
###
### distribution = "gaussian" is used for regression.
###
?gbm
set.seed(10)
gbm_concrete_1 = gbm(V785 ~ ., data = train, distribution = "bernoulli",
                     interaction.depth = 3, shrinkage = 0.01, bag.fraction = 0.5,
                     cv.folds = 3)
gbm.perf(gbm_concrete_1, method = "cv")

###
### What does it mean?
###

set.seed(10)
gbm_concrete_2 = gbm(V785 ~ ., data = train, distribution = "multinomial",
                     interaction.depth = 3, shrinkage = 0.01, bag.fraction = 0.5,
                     cv.folds = 3, n.tree = 100)
best_tree = gbm.perf(gbm_concrete_2, method = "cv")

###
### Final prediction:
###

gbm_y = predict(gbm_concrete_2, newdata = train, n.tree = best_tree)
a<-ceiling(gbm_y)-1
train_best<-colnames(gbm_y)[max.col(gbm_y,ties.method="first")]
get_error(a, train$V785)

gbm_y = predict(gbm_concrete_2, newdata = test, n.tree = best_tree)
round(gbm_y)
get_error(round(gbm_y), test$V785)


###############################################################################################
## Model 8. Your creation
##
library(class)
?knn

pre_train<-knn(train[-785],train[-785],train$V785,k=3)
get_error(pre_train, train$V785)

pre<-knn(train[-785],test[-785],train$V785,k=3)
get_error(pre, test$V785)

## set the k
iris_acc<-numeric() 
for(i in 1:20){
  pre_train<-knn(train[-785],test[-785],train$V785,k=i)
  x = table(pre_train, test$V785)
  iris_acc<-c(iris_acc, 1-sum(diag(x))/sum(x))
}
plot(iris_acc,type="l",ylab="Error Rate",
     xlab="K",main="Error Rate for Iris With Varying K")
