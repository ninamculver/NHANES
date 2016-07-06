library(MASS)
library(rpart)
library(rattle)
library(randomForest)
library(hydroGOF)
library(e1071)
library(kknn)
library(nnet)
library(BMA)
library(BMS)
library(gbm)

#------------------------------------------------#
#----------- Testing and Training Set -----------#
#------------------------------------------------#
#This is for the 70/30 split, however for the code ran I used the 1950-1969 and 1970-1999
#training =sample(nrow(temperature.data),nrow(temperature.data)*.7)
#temp.train = temperature.data[training,]
#temp.test = temperature.data[-training,]
#temp.testnoobs = temp.test[-1]

temp.train = temperature.data[temperature.data$year < 1970,]
temp.train$month = as.factor(temp.train$month)
temp.train$year = as.factor(temp.train$year)
temp.train = temp.train[-39]
temp.test = temperature.data[temperature.data$year >= 1970,]
temp.test$year = as.factor(temp.train$year)
temp.test$month = as.factor(temp.train$month)
temp.testobs = temp.test$orig
temp.testnoobs = temp.test[-1]

#-----------------------------------------------#
#---------------- Decision Tree ----------------#
#-----------------------------------------------#

temp.treemodel=rpart(orig ~.,data=temp.train)
fancyRpartPlot(temp.treemodel)
temp.treepred = predict(temp.treemodel, newdata = temp.test)
temp.treepred = as.factor(temp.treepred)
nrmse(temp.treepred, temp.testobs)
rmse(temp.treepred, temp.testobs)

plot(temp.testobs, temp.treepred)
m = (6:34)
lines(m,m,col="green")
#------------------------------------------------#
#----------------- Linear Model -----------------#
#------------------------------------------------#

temp.lmmodel = lm(orig~., data = temp.train)
temp.lmpred = predict(temp.lmmodel, newdata = temp.test)
temp.lmpred = as.numeric(temp.lmpred)
nrmse(temp.lmpred, temp.testobs)
rmse(temp.lmpred, temp.testobs)
plot(temp.testobs, temp.lmpred)
m = (6:34)
lines(m,m,col="green")

#------------- 1-1 Linear Modeling -------------#

acc101.lm = lm(orig~acc101, data = temperature.data)
summary(acc101.lm)
acc131.lm = lm(orig~acc101+ acc131, data = temperature.data)
summary(acc131.lm)
bcc11.lm = lm(orig~acc101+acc131+ bcc111, data = temperature.data)
summary(bcc11.lm)
bcc11m1.lm = lm(orig~acc101+ acc131+bcc111+bcc11m1, data = temperature.data)
summary(bcc11m1.lm)
cane.lm = lm(orig~acc101+acc131+bcc111+bcc11m1+cane, data = temperature.data)
summary(cane.lm)
ccsm41.lm = lm(orig~acc101+acc131+bcc111+bcc11m1+cane+ccsm41, data = temperature.data)
summary(ccsm41.lm)
cesbgc.lm = lm(orig~acc101+acc131+bcc111+bcc11m1+cane+ccsm41+ cesbgc, data = temperature.data)
summary(cesbgc.lm)
cesm1.lm = lm(orig~acc101+acc131+bcc111+bcc11m1+cane+ccsm41+cesbgc+cesm1, data = temperature.data)
summary(cesm1.lm)

#--- Checking Restraints on the Linear Model ---#

lmmodel = lm(temperature.data$orig~., data = temperature.data)
e = residuals(lmmodel)
es <-rstandard(lmmodel)
qqnorm(e)
qqnorm(es)
shapiro.test(e)
boxcox.results=boxcox(lmmodel)
cbind(boxcox.results$x,boxcox.results$y)
which.max(boxcox.results$y)
lambda=boxcox.results$x[which.max(boxcox.results$y)]
y.tilde=temperature.data$orig^lambda
x.data = temperature.data[-1]
trans.model=lm(y.tilde~., data = x.data)
e.tilde=residuals(trans.model)
qqnorm(e.tilde)
#NOTE: The residuals are not normal, therefore, our data is not linear. 

#-----------------------------------------------#
#---------------- Random Forest ----------------#
#-----------------------------------------------#
temp.forestmodel = randomForest(orig~., data = temp.train, ntree = 150)
temp.forestpred = predict(temp.forestmodel, newdata = temp.test)
temp.forestpred = as.numeric(temp.forestpred)
nrmse(temp.forestpred, temp.testobs)
rmse(temp.forestpred, temp.testobs)
plot(temp.testobs, temp.forestpred)
m = (6:34)
lines(m,m,col="green")

#------------------------------------------------#
#------------ Support Vector Machine ------------#
#------------------------------------------------#

#formula = temperature.data$orig ~ temperature.data$acc101 + temperature.data$acc131 + temperature.data$bcc111
#                        + temperature.data$bcc11m1 + temperature.data$cane + temperature.data$ccsm41 + temperature.data$cesbgc
#                       + temperature.data$cesm1 + temperature.data$ccmcc + temperature.data$cnrm + temperature.data$csiro
#                        + temperature.data$fgoals + temperature.data$fio + temperature.data$gfcm + temperature.data$gfesm2g
#                        + temperature.data$gfesm2m + temperature.data$gis2h + temperature.data$gis2r1 + temperature.data$gis2rcc + temperature.data$hagao 
#                        + temperature.data$hagcc + temperature.data$hages + temperature.data$inm + temperature.data$ipsl5alr + temperature.data$isplmr 
#                        + temperature.data$ipsl5blr + temperature.data$mri + temperature.data$mpilr + temperature.data$mpimr + temperature.data$miroc1 
#                        + temperature.data$miroc5 + temperature.data$mirochem + temperature.data$noresmm + temperature.data$noresmme + temperature.data$t
#                        + temperature.data$lat + temperature.data$long

#tuning = tune.svm(orig~., data=train, gamma=10^(-4:-1), cost=10^(1:4))
#This is commented out because tuning will take nearly a month to complete. 

temp.svmmodel = svm(orig~., data = temp.train )
temp.svmpred = predict(temp.svmmodel,newdata=temp.testobs)
svmpred = as.numeric(svmpred)
nrmse(svmpred, testorig)
rmse(temp.forestpred, temp.testobs)
plot(temp.testobs, temp.forestpred)
m = (6:34)
lines(m,m,col="green")


#-----------------------------------------------#
#------------- K-Nearest Neighbors -------------#
#-----------------------------------------------#

temp.knntrain = train.kknn(orig~., data = temp.train) #best kernel: optimal, best k = 5
temp.knnpred = (kknn(orig~., train = temp.train, test = temp.test, k=5, kernel = "optimal"))$fitted.values
nrmse(temp.knnpred, temp.testobs)
rmse(temp.knnpred, temp.testobs)
plot(temp.testobs, temp.knnpred)
m = (6:34)
lines(m,m,col="green")

#------------------------------------------------#
#---------------- Nueral Network ----------------#
#------------------------------------------------#

temp.nnetmodel = nnet(orig~., size = 9, data = train, linout = TRUE)
temp.nnetpred = predict(temp.nnetmodel, newdata = temp.test)
temp.nnetpred = as.numeric(temp.nnetpred)
nrmse(temp.nnetpred, temp.testobs)
rmse(temp.nnetpred, temp.testobs)
plot(temp.testobs, temp.nnetpred)
m = (6:34)
lines(m,m,col="green")
#-----------------------------------------------#
#---------- Bayesian Modeling Average ----------#
#-----------------------------------------------#

temp.bmamodel1 = bms(temp.train, mprior = "fixed", mprior.size =2, user.int = T)
temp.bmapred1 = predict(temp.bmamodel1, temp.testobs)
nrmse(temp.bmapred1, temp.testobs)
rmse(temp.bmapred1, temp.testobs)
plot(temp.testobs, temp.bmapred1)
m = (6:34)
lines(m,m,col="green")

temp.bmamodel2 = bms(temp.train, mprior = "uniform", g= "BRIC", burn = 10000, iter = 20000)
temp.bmapred2 = predict(temp.bmamodel2, temp.testobs)
nrmse(temp.bmapred2, temp.testobs)
rmse(temp.bmapred2, temp.testobs)
plot(temp.testobs, temp.bmapred2)
m = (6:34)
lines(m,m,col="green")

#-----------------------------------------------#
#---------- Gradient Boosting Machine ----------#
#-----------------------------------------------#

temp.gbmmodel = gbm(orig~., data = temp.train,
                distribution="bernoulli",
                n.trees=1000,
                shrinkage=0.05,
                interaction.depth=3,
                bag.fraction = 0.5, 
                train.fraction = 0.5, 
                n.minobsinnode = 10,
                cv.folds = 3,
                keep.data=TRUE,
                verbose=FALSE,
                n.cores=1)
temp.gbmpred = predict(temp.gbmmodel, newdata = temp.test)
#gbm.perf(gbm.model, 
#         plot.it = TRUE, 
#         oobag.curve = FALSE, 
#         overlay = TRUE, 
#         method = "OOB")
temp.gbmpred = as.numeric(temp.gbmpred)
nrmse(gbmpred, temp.testobs)
rmse(temp.gbmpred, temp.testobs)
plot(temp.testobs, temp.gbmpred)
m = (6:34)
lines(m,m,col="green")
