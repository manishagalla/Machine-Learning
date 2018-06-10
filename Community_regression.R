community <- read.csv("/Users/manishagalla/Documents/R/community", header=FALSE)
View(community)
colnames <- read.table("/Users/manishagalla/Documents/R/colnames", header=FALSE)
View(colnames)
colnames(community) = colnames$V2
View(community)
#Replacing "?" with NA
community[community=="?"] <- NA
#taking the columns which have number of NA values less than 25% of the total rows
myData <- community[,colSums(is.na(community))<0.25*nrow(community)]
attach(myData)
#sampling data into 75-25
instances <- sample(1:nrow(myData),size=0.75*nrow(myData))
train = myData[instances,]
test = myData[-instances,]

#neuralnet
nnet.fit <- nnet(ViolentCrimesPerPop~MedRent+RentLowQ+PctPersDenseHous+PctKids2Par+pctUrban+NumStreet+PctVacMore6Mos+HousVacant+PersPerRentOccHous+PersPerOccupHous+PctWorkMom+PctIlleg+whitePerCap+state+racepctblack+pctWWage,data=train, size=6)
nnet.predict <- predict(nnet.fit,newdata = test)
mse_nnet <- mean((nnet.predict - test$ViolentCrimesPerPop)^2)
mse_nnet
sqrt(mse_nnet)/mean(test$ViolentCrimesPerPop)

#linear Regression

model_new = lm(ViolentCrimesPerPop~MedRent+RentLowQ+PctPersDenseHous+PctKids2Par+pctUrban+NumStreet+PctVacMore6Mos+HousVacant+PersPerRentOccHous+PersPerOccupHous+PctWorkMom+PctIlleg+whitePerCap+state+racepctblack+pctWWage, data = train)
summary(model_new)
model_new_res = predict(model_new,newdata = test)
mse_lm_new = mean((test$ViolentCrimesPerPop-model_new_res)^2)
mse_lm_new
sqrt(mse_lm_new)/mean(test$ViolentCrimesPerPop)


#CV
dim(myData)
train_sample = sample(1994,1000)
model_new_train = lm(ViolentCrimesPerPop~MedRent+RentLowQ+PctPersDenseHous+PctKids2Par+pctUrban+NumStreet+PctVacMore6Mos+HousVacant+PersPerRentOccHous+PersPerOccupHous+PctWorkMom+PctIlleg+whitePerCap+state+racepctblack+pctWWage, data = myData,subset = train_sample)
summary(model_new_train)
mean(model_new_train$residuals^2)
sqrt(mean(model_new_train$residuals^2))/mean(myData$ViolentCrimesPerPop)
mean((ViolentCrimesPerPop - predict(model_new_train,myData))[-train_sample]^2)
sqrt(mean((ViolentCrimesPerPop - predict(model_new_train,myData))[-train_sample]^2))/mean(myData$ViolentCrimesPerPop)

#ridge
myData[is.na(myData)]=0
x <- model.matrix(ViolentCrimesPerPop~.,data = myData)
y <- myData$ViolentCrimesPerPop
train[is.na(train)]=0
set.seed(1)
grid = 10^seq(10,-2,length=100)
ridge.model = glmnet(x,y,alpha = 0,lambda = grid)
cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)
cv.out$lambda.min
min(cv.out$cvm)
train = myData[instances,]
x_new <- model.matrix(ViolentCrimesPerPop~.,data=train)
y_new <- train$ViolentCrimesPerPop
ridge.new = glmnet(x_new,y_new,alpha=0,lambda =cv.out$lambda.min)
test[is.na(test)]=0
x_test = model.matrix(ViolentCrimesPerPop~.,data = test)
ridge_res = predict(ridge.new,newx = x_test)
mse_ridge <- mean((test$ViolentCrimesPerPop-ridge_res)^2)
sqrt(mse_ridge)/mean(test$ViolentCrimesPerPop)



#lasso
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
cv.out$lambda.min
min(cv.out$cvm)
lasso.new = glmnet(x_new,y_new,alpha=1,lambda =cv.out$lambda.min)
lasso_res = predict(lasso.new,newx = x_test)
mse_lasso <- mean((test$ViolentCrimesPerPop-lasso_res)^2)
sqrt(mse_lasso)/mean(test$ViolentCrimesPerPop)

#glm
library("boot", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
train[is.na(train)]=0
data = sapply(train, as.numeric)
data = as.data.frame(data)
model_new_train_glm = glm(ViolentCrimesPerPop~.,data= data)
summary(model_new_train_glm)
cv.error_glm = cv.glm(data,model_new_train_glm)
cv.error_glm$delta[1]
sqrt(cv.error_glm$delta[1])/mean(ViolentCrimesPerPop)

#pcr
train = myData[instances,]
train[is.na(train)]=0
data = sapply(train, as.numeric)
data = as.data.frame(data)
data_test = sapply(test, as.numeric)
data_test = as.data.frame(data_test)
pcr.fit = pcr(ViolentCrimesPerPop~., data=data, scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")
#from validation graph 9 components gives best adjsted R2
model_pcr = lm(data$ViolentCrimesPerPop~., data = data[,1:9])
summary(model_pcr)
model_pcr_res = predict(model_pcr,newdata = data_test)
mse_pcr = mean((test$ViolentCrimesPerPop-model_pcr_res)^2)
sqrt(mse_pcr)/mean(test$ViolentCrimesPerPop)

#boosting
model_pcr = lm(ViolentCrimesPerPop~., data = myData[,1:9])
summary(model_pcr)
boost.comm = gbm(ViolentCrimesPerPop~. -communityname, data=train, distribution="gaussian", n.trees=500, shrinkage=.01)
boost.probs = predict(boost.comm,newdata=test,n.trees=500, type="response")
mse_boosting <- mean((test$ViolentCrimesPerPop-boost.probs)^2)
summary(test$ViolentCrimesPerPop)
sqrt(mse_boosting)/mean(test$ViolentCrimesPerPop)

#neuralnet
nnet.fit <- nnet(ViolentCrimesPerPop~MedRent+RentLowQ+PctPersDenseHous+PctKids2Par+pctUrban+NumStreet+PctVacMore6Mos+HousVacant+PersPerRentOccHous+PersPerOccupHous+PctWorkMom+PctIlleg+whitePerCap+state+racepctblack+pctWWage,data=train, size=6)
nnet.predict <- predict(nnet.fit,newdata = test)
mse_nnet <- mean((nnet.predict - test$ViolentCrimesPerPop)^2)
mse_nnet
sqrt(mse_nnet)/mean(test$ViolentCrimesPerPop)


