set.seed (10)
####################################################################################################
#Data prepare
#
library(plyr)
Data = read.csv("Concrete_Data.csv")

head(Data)
# rename colum and show the plot
myData<-rename(Data, c("Cement..component.1..kg.in.a.m.3.mixture."="x1",
                       "Blast.Furnace.Slag..component.2..kg.in.a.m.3.mixture."="x2",
                       "Fly.Ash..component.3..kg.in.a.m.3.mixture."="x3", 
                       "Water...component.4..kg.in.a.m.3.mixture." ="x4", 
                       "Superplasticizer..component.5..kg.in.a.m.3.mixture."  ="x5", 
                       "Coarse.Aggregate...component.6..kg.in.a.m.3.mixture." ="x6", 
                       "Fine.Aggregate..component.7..kg.in.a.m.3.mixture."="x7", 
                       "Age..day." ="x8", 
                       "Concrete.compressive.strength.MPa..megapascals.."="y"))
plot(myData)

# get the equal   size test and train dataset
nr <- nrow(myData)
df<-sample.int(n=nrow(myData),size = floor(.5*nrow(myData)), replace = F)

testData<-myData[-df,]                                        #test data
trainData<-myData[df,]                                        #train data 

####################################################################################################
#Model 1  	Linear regression with all the predictors (8)
library (boot)
lm<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8,data=trainData)
sqrt(mean(lm$residuals^2))                                    # train dateset RMSE 
sqrt(mean((testData$y-predict.glm(lm, testData[1:8]))^2))     # test dataset RMSE

cv.error= rep (0,10)                                          # 10-CV
for (i in 1:10) 
  {
    glm.fit=glm(y~x1+x2+x3+x4+x5+x6+x7+x8,data=trainData)
    cv.error[i]=cv.glm(trainData ,glm.fit ,K=10)$delta[1]
}
pred.train =predict(glm.fit,trainData,type = "response")
sqrt(mean((trainData$y-pred.train)^2))                        # RMSE 10-CV train dataset 

####################################################################################################
#Model 2  	Linear regression with variables selected using BIC 
library(leaps)
library(magrittr)
library(ggvis)

regfit.full = regsubsets(y ~ ., data = trainData, nvmax = 8)  #BIC selection
reg.summary = summary(regfit.full)

basline<-BIC(lm(y~1,data=trainData))                          #BIC for non-parameter model
reg.summary$bic                                               #BIC decrease value

plot(reg.summary$bic ,
     xlab="Number of Variables ",ylab="BIC",type='l')
spot.minBIC<-which.min(reg.summary$bic )                      # 6 variables are selected
points(spot.minBIC,
       reg.summary$bic [spot.minBIC],col="red",cex=2,pch=20)
plot(regfit.full,scale="bic")                                 # the variable subset is x in (1,2,3,4,5,8)


coefi =coef(regfit.full ,id=5)
train.mat = model.matrix(y ~., data=trainData)
pred.train = train.mat[,names(coefi)]%*%coefi
sqrt(mean((trainData$y-pred.train)^2))                        #RMSE train dataset

test.mat = model.matrix(y ~., data=testData)
pred.test = test.mat[,names(coefi)]%*%coefi 
sqrt(mean((testData$y-pred.test)^2))                          #RMSE test dataset

folds = sample(1:10,nrow(trainData),replace=TRUE)             #10 CV
table(folds)

predict.regsubsets =function (object ,newdata ,id ,...)
{
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi) 
  mat[,xvars]%*%coefi
}

cv.errors=matrix(NA,10,8, dimnames=list(NULL, paste(1:8)))    #
for(j in 1:10){
  best.fit = regsubsets(y ~., data=trainData[folds != j,], nvmax = 8)
  for (i in 1:8){
    pred = predict.regsubsets(best.fit, trainData[folds == j, ], id = i)
    cv.errors[j, i] = sqrt(mean((trainData$y[folds == j] - pred)^2))
  }
}
rmean.cv.errors = apply(cv.errors ,2,mean)
rmean.cv.errors
plot(rmean.cv.errors, pch = 8, type = "b")
points(6,rmean.cv.errors[6],col="red",cex=2,pch=20)
reg.best=regsubsets (y~.,data=trainData , nvmax=8)

coef(reg.best ,which.min(rmean.cv.errors))                   # best model from 10-CV
min(rmean.cv.errors)                                         # RMSE 10-CV train dataset
####################################################################################################
#Model 3  	Lasso regression  
library(lars)
#install.packages("lars")
train.mat<-model.matrix(y~.,trainData)
lm.lasso<-lars(train.mat,trainData$y, type="lasso")
lm.lasso
plot(lm.lasso)
summary(lm.lasso)
cva <- cv.lars(train.mat, trainData$y, K = 10, plot.it = TRUE) 

sqrt(mean(cva$cv))

best <- cva$index[which.min(cva$cv)] 
best
coef <- coef.lars(lm.lasso, mode = "fraction", s = best) #使得CV最小时的系数 

coef[coef != 0] #通过CV选择的变量


lm.lasso$Cp[which.min(lm.lasso$Cp)]
coef1 <- coef.lars(lm.lasso, mode = "step", s = 10) #使得Cp最小时的系数（s=15） 
coef1[coef1 != 0] #通过CV选择的变量 

y_fit=predict.lars(object=lm.lasso,newx=train.mat,s=10,mode="step")
y_hat=predict.lars(object=lm.lasso,newx=train.mat,s=best,mode="fraction")
mat=cbind(trainData$y,y_hat$fit,y_fit$fit)
sqrt(mean((trainData$y-y_hat$fit)^2))

sqrt(mean((trainData$y-y_fit$fit)^2))

matplot(mat,type='l',col=c("black","red","green"),main="In Sample Plot",ylab="HAM2")
legend("topright",legend=c("Actual","Predicted","by step"),fill=c("black","red","green"),bty='n')
grid()

############################################s########################################################
#Model 4  	Ridge regression
library(MASS)
lm.ridge <- lm.ridge(formula=y~.,data=trainData,lambda=seq(from=0,to=300,by=1))
#lm.ridge$coef
plot(lm.ridge)
#plot this information
while("draw plot"!=0){
  matplot(x=t(lm.ridge$coef),type='s',
          main="Ridge Regression Lambda vs Coefficient Plot",
          xlab="Lambda Values",
          ylab="Coefficients",
          col=c("black","red","blue","green","pink","orange","yellow","brown"))
  grid()
  legend("topright",legend=rownames(lm.ridge$coef),  
         fill=c("black","red","blue","green","pink","orange","yellow","brown"),
         bty='n',
         cex=0.5)
  break
}


####################################################################################################
#Model 5  	 Self-selet model: elastic net 
library(glmnet)
train.mat<-model.matrix(y~.,trainData)[,-1]
train.mat
lm.elnet <- glmnet(train.mat, trainData$y, family="gaussian", alpha=.3)
plot(lm.elnet)
lm.elnet
y_train<-predict(lm.elnet, s=0,newx=train.mat)
rmse <- sqrt(mean((trainData$y - y_train)^2))
rmse
fit0$lambda.1se

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(train.mat, trainData$y, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}
par(mfrow=c(1,1))
# For plotting options, type '?plot.glmnet' in R console


plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")

findrmse=function(train.mat,trainData,...){
    
  
  yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=train.mat)
  yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=train.mat)
  yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=train.mat)
  yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=train.mat)
  yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=train.mat)
  yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=train.mat)
  yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=train.mat)
  yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=train.mat)
  yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=train.mat)
  yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=train.mat)
  yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=train.mat)
  
  mse0 <- sqrt(mean((trainData$y - yhat0)^2))
  mse1 <- sqrt(mean((trainData$y - yhat1)^2))
  mse2 <- sqrt(mean((trainData$y - yhat2)^2))
  mse3 <- sqrt(mean((trainData$y - yhat3)^2))
  mse4 <- sqrt(mean((trainData$y - yhat4)^2))
  mse5 <- sqrt(mean((trainData$y - yhat5)^2))
  mse6 <- sqrt(mean((trainData$y - yhat6)^2))
  mse7 <- sqrt(mean((trainData$y - yhat7)^2))
  mse8 <- sqrt(mean((trainData$y - yhat8)^2))
  mse9 <- sqrt(mean((trainData$y - yhat9)^2))
  mse10 <- sqrt(mean((trainData$y - yhat10)^2))
  y=c(mse0,mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10)
  print(y)
  plot(x=c(0:10),y,type="b")
}

y

test.mat<-model.matrix(y~.,testData)[,-1]
findrmse(test.mat,testData)
