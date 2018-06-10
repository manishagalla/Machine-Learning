library(rpart)
library(ggplot2)
library(lattice)
library(mlbench)
library(caret)
library(ada)
library(class)
library(caret)
library(lattice)
library(boot)
library(stats)
library(pROC)
library(nutshell)
library(adabag)
library(randomForest)
#KNN
spam_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data", header=FALSE)
spam_dataframe <- as.data.frame(spam_data)
maxs <-	apply(spam_dataframe,	MARGIN	=	2,	max)
mins <- apply(spam_dataframe, MARGIN=2,min)
scaled <- spam_dataframe
scaled <-	 as.data.frame(scale(spam_dataframe,	center	=	TRUE,	scale	=	maxs	- mins))
col_val <- as.integer(ncol(spam_dataframe))
y <- scaled[,col_val]
set.seed(1)
idx <- createFolds(y, k=2)
accuracy_knn <- c()
area_curve_knn <- c()
precision_knn <- c()
recall_knn <- c()
for (i in 1:length(idx)) {
  trainInstances <- scaled[ -idx[[i]] , ]
  trainInstances <- na.omit(trainInstances)
  testInstances <- scaled[idx[[i]] ,]
  testInstances <- na.omit(testInstances)
  lr_pred <- knn(train=trainInstances[,1:col_val-1],test=testInstances[,1:col_val-1],cl=trainInstances[,col_val],k=5,l=1)
  knn_accuracy <- mean(lr_pred==y[idx[[i]]])
  tab <- table(lr_pred,y[idx[[i]]])
  area <- roc(lr_pred,y[idx[[i]]])
  prcn <- precision(tab)
  recl <- recall(tab)
  accuracy_knn[i] <- knn_accuracy
  area_curve_knn[i] <- area$auc
  precision_knn[i] <- prcn
  recall_knn[i] <- recl
  
}
cat("accuracy for knn is:",mean(accuracy_knn)*100)
cat("\n")
cat("recall for knn is:",mean(recall_knn)*100)
cat("\n")
cat("precision for knn is:",mean(precision_knn)*100)
cat("\n")
cat("areac under the curve for knn is:", mean(area_curve_knn)*100)
cat("\n")
cat("###############################################")
cat("\n")

#Boosting
#spam_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data", header=FALSE)
spam_dataframe <- as.data.frame(spam_data)
maxs <-	apply(spam_dataframe,	MARGIN	=	2,	max)
mins <- apply(spam_dataframe, MARGIN=2,min)
#instances <- sample(1:nrow(spam_data),size=0.8*nrow(spam_data))
scaled <-	 as.data.frame(scale(spam_dataframe,	center	=	TRUE,	scale	=	maxs	- mins))
scaled <- na.omit(scaled)
col_val <- as.integer(ncol(scaled))
y <- scaled[,col_val]
set.seed(1)
idx <- createFolds(y, k=10)
accuracy_boosting <- c()
precision_boosting <- c()
recall_boosting <- c()
for (i in 1:length(idx)) {
  trainInstances <- scaled[ -idx[[i]] , ]
  trainInstances <- na.omit(trainInstances)
  testInstances <- scaled[idx[[i]] ,]
  testInstances <- na.omit(testInstances)
  fit_boost <- ada(V58~.,data = trainInstances,loss="exponential",iter=70,bag.frac=2.5)
  pred_boost <- predict(fit_boost,newdata=testInstances)
  accuracy_boost <- mean(pred_boost==testInstances$V58)
  tab <- table(pred_boost,testInstances$V58)
  prcn <- precision(tab)
  recl <- recall(tab)
  accuracy_boosting[i] <- accuracy_boost
  precision_boosting[i] <- prcn
  recall_boosting[i] <- recl
}
cat("accuracy for boosting is:",mean(accuracy_boosting)*100)
cat("\n")
cat("recall for boosting is:",mean(recall_boosting)*100)
cat("\n")
cat("precision for boosting is:",mean(precision_boosting)*100)
cat("\n")
cat("###############################################")
cat("\n")

#Bagging
#spam_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data", header=FALSE)
data("spambase")
col_val <- as.integer(ncol(spambase))
y <- spambase[,col_val]
set.seed(100)
idx <- createFolds(y, k=10)
accuracy_bagging <- c()
precision_bagging <- c()
recall_bagging <- c()
for (i in 1:length(idx)) {
  trainInstances <- spambase[ -idx[[i]] , ]
  trainInstances <- na.omit(trainInstances)
  testInstances <- spambase[idx[[i]] ,]
  testInstances <- na.omit(testInstances)
  fit_bag <- bagging(formula=is_spam~.,data=trainInstances,mfinal =100,control=rpart.control(cp=0.01, minsplit=20))
  pred_bag <- predict(fit_bag,newdata=testInstances)
  accuracy_bag <- mean(pred_bag$class==testInstances$is_spam)
  tab <- table(pred_bag$class,testInstances$is_spam)
  prcn <- precision(tab)
  recl <- recall(tab)
  accuracy_bagging[i] <- accuracy_bag
  precision_bagging[i] <- prcn
  recall_bagging[i] <- recl
}
cat("accuracy for bagging is:",mean(accuracy_bagging)*100)
cat("\n")
cat("recall for bagging is:",mean(recall_bagging)*100)
cat("\n")
cat("precision for bagging is:",mean(precision_bagging)*100)
cat("\n")
cat("###############################################")
cat("\n")

#Logistic Regression
#spam_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data", header=FALSE)
spam_dataframe <- as.data.frame(spam_data)
spam_dataframe < na.omit(spam_dataframe)
sc <- spam_dataframe[,1:57]
maxs <-	apply(sc,	MARGIN	=	2,	max)
mins <- apply(sc, MARGIN=2,min)
scaled <-	 as.data.frame(scale(sc,	center	=	TRUE,	scale	=	maxs	- mins))
y <- spam_dataframe[,length(spam_data)]
x <- cbind.data.frame(scaled,y)
fit_LR <- glm(y ~ .,family = binomial(link = "logit"),data=x)
cv_lr <- cv.glm(x,fit_LR,K=10)
accuracy_lr <- (1-cv_lr$delta)*100
cat ("accuracy of lr: ",accuracy_lr[1])
cat("\n")
cat("###############################################")
cat("\n")

#Random Forest
#spam_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data", header=FALSE)
spam_dataframe <- as.data.frame(spam_data)
col_val <- as.integer(ncol(spam_dataframe))
spam_dataframe$V58 <- as.factor(spam_dataframe$V58)
pred_rf <- rfcv(spam_dataframe[1:col_val-1],spam_dataframe[,col_val],cv.fold = 10, scale="log", step=0.5)
rf_accuracy <- (1 - pred_rf$error.cv[1])*100
tab <- table(pred_rf$predicted$`57`,spam_dataframe[,col_val])
prcn <- precision(tab)
recl <- recall(tab)
#area <- roc(pred_rf,spam_dataframe[,col_val])
cat("accuracy of rf: ",rf_accuracy)
cat("\n")
cat("recall for rf is:",prcn*100)
cat("\n")
cat("precision for rf is:",recl*100)
cat("\n")
#cat("areac under the curve for rf is:", area)
cat("\n")
