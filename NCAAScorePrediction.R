#Part-1
RegularSeasonDetailedResults <- read.csv("/Users/manishagalla/Documents/R/RegularSeasonDetailedResults.csv")
View(RegularSeasonDetailedResults)
TourneyCompactResults <- read.csv("/Users/manishagalla/Documents/R/TourneyCompactResults.csv")
View(TourneyCompactResults)
myData = RegularSeasonDetailedResults
library("dplyr", lib.loc="~/R/win-library/3.4")
aggdata_l <- aggregate(list(myData$Lfgm,myData$Lfga,myData$Lfgm3,myData$Lfga3,myData$Lftm,myData$Lfta,myData$Lor,myData$Ldr,myData$Last,myData$Lto,myData$Lstl,myData$Lblk,myData$Lpf), by=list(myData$Season,myData$Lteam), FUN=sum, na.rm= TRUE)
View(aggdata_l)
aggdata_w <- aggregate(list(myData$Wfgm,myData$Wfga,myData$Wfgm3,myData$Wfga3,myData$Wftm,myData$Wfta,myData$Wor,myData$Wdr,myData$Wast,myData$Wto,myData$Wstl,myData$Wblk,myData$Wpf), by=list(myData$Season,myData$Wteam), FUN=sum, na.rm= TRUE)
colnames(aggdata_w) <- c("Season","Team","Wfgm","Wfga","Wfgm3","Wfga3","Wftm","Wfta","Wor","Wdr","Wast","Wto","Wstl","Wblk","Wpf")
colnames(aggdata_l) <- c("Season","Team","Lfgm","Lfga","Lfgm3","Lfga3","Lftm","Lfta","Lor","Ldr","Last","Lto","Lstl","Lblk","Lpf")
View(aggdata_w)
aggdata_lw <- merge(aggdata_l,aggdata_w,by=c("Season","Team"))
View(aggdata_lw)
? aggregate
df1 <- count(myData,c("Season","Wteam"))
library("plyr", lib.loc="~/R/win-library/3.4")
df1 <- count(myData,c("Season","Wteam"))
View(df1)
df2 <- count(myData,c("Season","Lteam"))
names(df1) = c("Season","Team","count_w")
names(df2) = c("Season","Team","count_l")
aggdata_lw <- merge(aggdata_lw,df1,by=c("Season","Team"))
aggdata_lw <- merge(aggdata_lw,df2,by=c("Season","Team"))
aggdata_avg <- data.frame()
aggdata_avg = aggdata_lw
aggdata_avg$Avgfgm <- (aggdata_lw$Lfgm + aggdata_lw$Wfgm)/(aggdata_lw$count_l+ aggdata_lw$count_w)
aggdata_avg$Avgfga <- (aggdata_lw$Lfga + aggdata_lw$Wfga)/(aggdata_lw$count_l+ aggdata_lw$count_w)
aggdata_avg$Avgfgm3 <- (aggdata_lw$Lfgm3 + aggdata_lw$Wfgm3)/(aggdata_lw$count_l+ aggdata_lw$count_w)
aggdata_avg$Avgfga3 <- (aggdata_lw$Lfga3 + aggdata_lw$Wfga3)/(aggdata_lw$count_l+ aggdata_lw$count_w)
aggdata_avg$Avgftm <- (aggdata_lw$Lftm + aggdata_lw$Wftm)/(aggdata_lw$count_l+ aggdata_lw$count_w)
aggdata_avg$Avgfta <- (aggdata_lw$Lfta + aggdata_lw$Wfta)/(aggdata_lw$count_l+ aggdata_lw$count_w)
aggdata_avg$Avgor <- (aggdata_lw$Lor + aggdata_lw$Wor)/(aggdata_lw$count_l+ aggdata_lw$count_w)
aggdata_avg$Avgdr <- (aggdata_lw$Ldr + aggdata_lw$Wdr)/(aggdata_lw$count_l+ aggdata_lw$count_w)
aggdata_avg$Avg_ast <- (aggdata_lw$Last + aggdata_lw$Wast)/(aggdata_lw$count_l+ aggdata_lw$count_w)
aggdata_avg$Avg_to <- (aggdata_lw$Lto + aggdata_lw$Wto)/(aggdata_lw$count_l+ aggdata_lw$count_w)
aggdata_avg$Avg_stl <- (aggdata_lw$Lstl + aggdata_lw$Wstl)/(aggdata_lw$count_l+ aggdata_lw$count_w)
aggdata_avg$Avg_blk <- (aggdata_lw$Lblk + aggdata_lw$Wblk)/(aggdata_lw$count_l+ aggdata_lw$count_w)
aggdata_avg$Avg_pf <- (aggdata_lw$Lpf + aggdata_lw$Wpf)/(aggdata_lw$count_l+ aggdata_lw$count_w)
keep_list = c("Season","Team","Avgfgm","Avgfga","Avgfgm3","Avgfga3","Avgftm","Avgfta","Avgor","Avgdr","Avg_ast","Avg_to","Avg_stl","Avg_blk","Avg_pf")
aggdata_final <- aggdata_avg[keep_list]
View(aggdata_final)

#Part-2 Clustering analysis
k.max <- 15
data <- aggdata_final[,3:15]
wss <- sapply(1:k.max,
function(k){kmeans(data, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
type="b", pch = 19, frame = FALSE,
xlab="Number of clusters K",
ylab="Total within-clusters sum of squares")
km_data = kmeans(data,4)
km_data
scaled_data <- scale(data)

wss <- sapply(1:k.max,
function(k){kmeans(scaled_data, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
type="b", pch = 19, frame = FALSE,
xlab="Number of clusters K",
ylab="Total within-clusters sum of squares")


data.complete = hclust(dist(scaled_data), method="complete")
data.ward = hclust(dist(scaled_data), method="ward.D2")
data.average = hclust(dist(scaled_data), method="average")
par(mfrow=c(1,3))
plot(data.complete,main="Complete Linkage", xlab="", sub="", cex=.9,labels = FALSE)

plot(data.ward, main="ward.D2 Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(data.average, main="Average Linkages", xlab="", sub="", cex=.9,labels = FALSE)



Pca_data=prcomp(data, scale. = TRUE, center = TRUE)
plot(Pca_data)
Pca_data.var =Pca_data$sdev ^2
pve=Pca_data.var/sum(Pca_data.var )
plot(pve , xlab=" Principal Component ", ylab=" Proportion of
Variance Explained ", ylim=c(0,1) ,type='b')
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
type='b')


Pca_data.scaled = scale(Pca_data$x[,1:2])
Pca_data.ward = hclust(dist(Pca_data.scaled), method="ward.D2")
plot(Pca_data.ward,main="Ward Linkage", xlab="", sub="", cex=.9)



wss <- sapply(1:k.max,
              function(k){kmeans(Pca_data$x[,1:2], k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


km_stones_pca = kmeans(Pca_data$x[,1:4],4)
km_stones_pca
km_stones_pca = kmeans(Pca_data$x[,1:4],4)
km_stones_pca = kmeans(Pca_data$x[,1:3],4)
km_stones_pca
km_stones_pca = kmeans(Pca_data$x[,1:2],4)
km_stones_pca
km_stones_pca = kmeans(Pca_data$x[,1],4)
km_stones_pca

#Part-3 Regression model
dataframe= as.data.frame( TourneyCompactResults[,c(1,3,4)], drop=false)
View(dataframe)
dataframe1= as.data.frame( TourneyCompactResults[,c(1,5,6)], drop=false)
View(dataframe1)
colnames(dataframe) <- c("Season","Team","Score")
colnames(dataframe1) <- c("Season","Team","Score")

score <- rbind(dataframe,dataframe1)
View(score)
final_score = score[score$Season>=2003]
final_score = score[score$Season>=2003,]
View(final_score)

regression_data = merge(final_score,aggdata_final,by=c("Season","Team"))
View(regression_data)


#Linear Regression

pred_regress_data = regression_data[-c(1,2)]
lr.model_score = lm(Score~., data=pred_regress_data)
summary(lr.model_score)
lr.model_score_update = lm(Score~. -Avgfga -Avgfgm3 -Avgfga3 -Avgfta -Avgdr -Avg_ast -Avg_stl -Avg_blk -Avg_pf, data=pred_regress_data)
summary(lr.model_score_update)
plot(lr.model_score$residuals~lr.model_score$fitted.values)
plot(lr.model_score_update$residuals~lr.model_score_update$fitted.values)
qqnorm(lr.model_score$residuals)
qqline(lr.model_score$residuals)
qqnorm(lr.model_score_update$residuals)
qqline(lr.model_score_update$residuals)
sqrt(mean(lr.model_score$residuals^2))/mean(pred_regress_data$Score) 


#CV
dim(pred_regress_data)
train = sample(1821,1000)
lr.model_score_train = lm(Score~. -Avgfga -Avgfgm3 -Avgfga3 -Avgfta -Avgdr -Avg_ast -Avg_stl -Avg_blk -Avg_pf, data=pred_regress_data,subset=train)
#lr.model_score_update = lm(Score~. -Avgfga -Avgfgm3 -Avgfga3 -Avgfta -Avgdr -Avg_ast -Avg_stl -Avg_blk -Avg_pf, data=pred_regress_data)

summary(lr.model_score_train)
qqnorm(lr.model_score_train$residuals)
qqline(lr.model_score_train$residuals)
mean(lr.model_score_train$residuals^2) #Training MSE
sqrt(mean(lr.model_score_train$residuals^2))/mean(pred_regress_data$Score)
attach(pred_regress_data)
mean((Score - predict(lr.model_score_train_glm,pred_regress_data))[-train]^2) #Test MSE
sqrt(mean((Score - predict(lr.model_score_train,pred_regress_data))[-train]^2))/mean(pred_regress_data$Score)

#Generalized linera oderl
lr.model_score_train_glm = glm(Score~., data=pred_regress_data)
summary(lr.model_score_train_glm)
cv.error_glm = cv.glm(pred_regress_data,lr.model_score_train_glm)
cv.error_glm$delta

# Next we will try Ridge and Lasso. Note we use the built
# in CV function to select the best lambda, and this will
# also give the test MSE error estimate for that lambda.


x=model.matrix(Score~.,pred_regress_data)[,-1]
y=pred_regress_data$Score
grid=10^seq(10,-2,length=100)

ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)
cv.out$lambda.min
min(cv.out$cvm)

  lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
  cv.out=cv.glmnet(x,y,alpha=1)
  plot(cv.out)
  cv.out$lambda.min
  min(cv.out$cvm)


#PCR
pcr.fit = pcr(Score~., data=pred_regress_data, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

# Now let's try KNN. 
#k-fold CV for different values of K

bins = sample(1:10,1821, replace = TRUE)
binErrs = rep(0,10)
errs = rep(0,20)

errs = rep(0,20)
for(i in 1:20){
for(k in 1:10){
train.x = scale(x[bins != k,])
test.x = scale(x[bins == k,])
train.y = y[bins != k]
test.y = y[bins == k]
knn.fit = knn.reg(train.x, test.x, train.y, k=i)
binErrs[k] = mean((test.y - knn.fit$pred)^2)
}
errs[i] = mean(binErrs)
}
plot(errs,xlab="K value",
     ylab="MSE")







