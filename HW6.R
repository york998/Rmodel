setwd("SDS\\HW6")
num_rows = 1600
train = read.csv("train.csv", header = F, nrows = num_rows)
test = read.csv("test.csv", header = F,nrows =400)
train$V785 = as.factor(train$V785)
summary(train$V785)/num_rows
dim(train)
###
###Show image
###
###

show_an_image = function(n)
{
  v  = as.numeric(train[n,1:784])
  im = matrix(v,28,28)
  im = im[,nrow(im):1]
  image(im, col = gray((0:255)/255), main = train[n,785])
}

par(mfrow = c(2, 3))
for (i in c(1 , 146 , 291 , 437,  582 , 727,  873, 1018, 1163, 1309, 1454, 1600))
{
  show_an_image(i)
}





################################################################################################

# load the libraries
library(mlbench)

# summarize dataset
sum<-summary(train[,10:20])
train.small <- train
reduce<-c()
for(i in c(1:784)){
  if(min(train[i])==max(train[i])){
   reduce<- c(reduce, i)
  }
}
train.small=train[,-reduce]  

################################################################################################



################################################################################################
## Model 1. LDA
##
head(train,3)
library (MASS)
#train1=train$V785
#train1$V785 = ifelse( train1$V785 == "1", 1, 0)
install.packages(Hmisc)
r2 <- lda(formula = V785 ~ ., 
          data = train, 
          prior = rep(1,10)/10,
          CV = TRUE)
r <- lda(V785 ~ ., data = train )
r$prior
names(train)
##################################################################################
library(caret)
library(vegan)
vars_check <- nearZeroVar(train[-785], saveMetrics = TRUE, names = TRUE)
pca_pre=function(train){
  

dist_matrix <- vegdist(train[-785],
                       method = "euclidean",
                       binary = FALSE,
                       diag = FALSE,
                       upper = FALSE,
                       na.rm = TRUE)

PCA_vars <- cmdscale(d = dist_matrix,
                     k = 5,
                     eig = TRUE,
                     add = FALSE,
                     x.ret = FALSE)
return(PCA_vars)
}
a <- PCA_vars$points
a <- cbind(a,train$V785)
a<-as.data.frame(a)
lda.fit<-lda(V6 ~ ., data = a )

LDA_pldist <- lda(x = PCA_vars$points,
                  grouping = train$V785)
LDA_pldist <- lda(x = PCA_vars$points,
                  grouping = train$group,
                  tol = 1.0e-25)
salmon1 <- predict(LDA_pldist)
confus_m <- table(train$V785, salmon1$class)

1-sum(diag(confus_m ))/sum(confus_m)                         ######train error

###################################################################################
lda_predictions = predict(lda_model)$class

get_error(salmon1$class , train$V785)
get_error = function(predicted, observed)
{
  x = table( predicted, observed)
  print(x)
  cat("\n")
  cat( 100 * round( 1-sum(diag(x))/sum(x), 4), sep = "", "%\n\n")
}
testnew=pca_pre(test)$points

a <- testnew
a <- cbind(a,test$V785)
a<-as.data.frame(a)
lda_test_predictions = predict(lda.fit, newdata = a)$class
qda_test_predictions = predict(qda_model, newdata = test)$class

get_error(lda_test_predictions , test$V785)

get_error(qda_test_predictions , test$yesno)

################################################################################################
## Model 2. QDA
##

qda_model = qda(V785 ~ . , data = train.samll)
qda_model

qda_predictions = predict(qda_model)$class

get_error( qda_predictions , train$yesno )


################################################################################################
## Model 3. Na??ve Bayes
##

################################################################################################
## Model 4. SVM
##
svm_model <- svm(V785 ~ ., data=train)

################################################################################################
## Model 5. Single Tree
##

################################################################################################
## Model 6. Random Forest
##

################################################################################################
## Model 7. Gradient Boosting
##

################################################################################################
## Model 8. Your creation
##
