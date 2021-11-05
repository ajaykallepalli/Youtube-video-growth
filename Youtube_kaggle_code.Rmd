---
title: "Youtube Kaggle Code"
author: "Ajay Kallepalli"
date: "11/4/2021"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Final code for Youtube Kaggle competition predictions

#### Required Libraries  
```{r}
library(stringr)
library(reshape2)
library(ggplot2)
library(caret)
library(glmnet)
library(pls)
library(randomForest)
library(knitr)
```

#### Methodology: Preprocessing

1.1 Simple Feature Creation  
```{r}
setwd("C:\\Users\\Ajay Kallepalli\\Documents\\GitHub\\Youtube-video-growth")
prj <- read.csv("101C_Final-Project.csv", row.names = 1, na.strings=c("","NA"))
### Published Date
date_chr <- as.character(prj$PublishedDate)
# Month
month <- str_extract(date_chr, "^[\\d]{1,2}")
month <- factor(month, labels = c("April", "May", "June", "July", "August", "Septemb
er"))
month <- as.numeric(month)
# Day
day <- str_extract(date_chr, "(?<=/)\\d{1,2}(?=/)")
day <- as.numeric(day)
# Weekday
date <- as.Date(str_extract(date_chr, ".*(?=\\s)"), "%m/%d/%y")
weekday <- as.POSIXlt(date)$wday
# Weekend or Not
weekend <- rep(0, length(date_chr))
weekend[weekday == 0 | weekday == 6] <- 1 # 1 if weekend
# Time
time <- str_extract(date_chr, "\\d{1,2}:\\d{1,2}")
# Hour
hour <- str_extract(date_chr, "\\d{1,2}(?=:)")
hour <- as.numeric(hour)
# Minute
minute <- str_extract(date_chr, "(?<=:)\\d{1,2}")
minute <- as.numeric(minute)
# xth minute of day
min_day <- (hour * 60) + minute
# xth minute after 2 hours
min_2hours <- min_day + 120
for (i in 1:length(min_2hours)) {
 if(min_2hours[i] >= 1440) min_2hours[i] <- min_2hours[i] - 1440
}

### Combine low/high binary features and removing them
# subscribers
a <- b <- rep(0,nrow(prj))
a[as.logical(prj$Num_Subscribers_Base_low_mid)] <- 2
b[as.logical(prj$Num_Subscribers_Base_mid_high)] <- 3
sub_count <- prj$Num_Subscribers_Base_low + a + b
sub_count[sub_count==0] <- 4
# views
a <- b <- rep(0,nrow(prj))
a[as.logical(prj$Num_Views_Base_low_mid)] <- 2
b[as.logical(prj$Num_Views_Base_mid_high)] <- 3
view_count <- prj$Num_Views_Base_low + a + b
view_count[view_count==0] <- 4
# average growth
a <- b <- rep(0,nrow(prj))
a[as.logical(prj$avg_growth_low_mid)] <- 2
b[as.logical(prj$avg_growth_mid_high)] <- 3
avg_growth <- prj$avg_growth_low + a + b
avg_growth[avg_growth==0] <- 4
# video count
a <- b <- rep(0,nrow(prj))
a[as.logical(prj$count_vids_low_mid)] <- 2
b[as.logical(prj$count_vids_mid_high)] <- 3
video_count <- prj$count_vids_low + a + b
video_count[video_count==0] <- 4
# removal
prj <- prj[,-(247:258)]
### Combine New Features / Remove Published Date
prj <- cbind(sub_count,view_count,avg_growth,video_count,month,
 day,weekday,weekend,minute,min_2hours,min_day,prj)
prj <- prj[,-which(colnames(prj) == "PublishedDate")]

```

1.2 Data Cleaning  
```{r}
### Remove featuers that have same values for all observations
keep <- sapply(prj, function(x){length(unique(x))>1})
prj <- prj[,keep]
### Remove features that are very unbalanced
remove <- sapply(prj, function(x){length(table(x)) == 2 & any(table(x)<10)})
prj <- prj[,!remove]
### Finding outliers and removing them
prj <- prj[-c(6514),]
lm_fit <- lm(growth_2_6~., prj)
cooks <- cooks.distance(lm_fit)
out <- cooks > (4/nrow(prj))
prj <- prj[!out,]

```

1.3 Same preprocessing on test data
```{r}
setwd("C:\\Users\\Ajay Kallepalli\\Documents\\GitHub\\Youtube-video-growth")
tst <- read.csv("101C_Final_Test.csv", row.names = 1, na.strings=c("","NA"))
### Data Exploration ``
### Published Date
date_chr <- as.character(tst$PublishedDate)
# Month
month <- str_extract(date_chr, "^[\\d]{1,2}")
month <- factor(month, labels = c("April", "May", "June", "July", "August", "Septemb
er"))
month <- as.numeric(month)
# Day
day <- str_extract(date_chr, "(?<=/)\\d{1,2}(?=/)")
day <- as.numeric(day)
# Weekday
date <- as.Date(str_extract(date_chr, ".*(?=\\s)"), "%m/%d/%y")
weekday <- as.POSIXlt(date)$wday
# Weekend or Not
weekend <- rep(0, length(date_chr))
weekend[weekday == 0 | weekday == 6] <- 1 # 1 if weekend
# Time
time <- str_extract(date_chr, "\\d{1,2}:\\d{1,2}")
# Hour
hour <- str_extract(date_chr, "\\d{1,2}(?=:)")
hour <- as.numeric(hour)
# Minute
minute <- str_extract(date_chr, "(?<=:)\\d{1,2}")
minute <- as.numeric(minute)
# xth minute of day
min_day <- (hour * 60) + minute
# xth minute after 2 hours
min_2hours <- min_day + 120
for (i in 1:length(min_2hours)) {
 if(min_2hours[i] >= 1440) min_2hours[i] <- min_2hours[i] - 1440
}
# What part of the hour
# Part of day
### Combine low/high binary features and removing them
# subscribers
a <- b <- rep(0,nrow(tst))
a[as.logical(tst$Num_Subscribers_Base_low_mid)] <- 2
b[as.logical(tst$Num_Subscribers_Base_mid_high)] <- 3
sub_count <- tst$Num_Subscribers_Base_low + a + b
sub_count[sub_count==0] <- 4
# views
a <- b <- rep(0,nrow(tst))
a[as.logical(tst$Num_Views_Base_low_mid)] <- 2
b[as.logical(tst$Num_Views_Base_mid_high)] <- 3
view_count <- tst$Num_Views_Base_low + a + b
view_count[view_count==0] <- 4
# average growth
a <- b <- rep(0,nrow(tst))
a[as.logical(tst$avg_growth_low_mid)] <- 2
b[as.logical(tst$avg_growth_mid_high)] <- 3
avg_growth <- tst$avg_growth_low + a + b
avg_growth[avg_growth==0] <- 4
# video count
a <- b <- rep(0,nrow(tst))
a[as.logical(tst$count_vids_low_mid)] <- 2
b[as.logical(tst$count_vids_mid_high)] <- 3
video_count <- tst$count_vids_low + a + b
video_count[video_count==0] <- 4
### Combine New Features / Remove Published Date
tst <- cbind(sub_count,view_count,avg_growth,video_count,month,
 day,weekday,weekend,minute,min_day,min_2hours,tst)
tst <- tst[,-which(colnames(tst) == "PublishedDate")]
### keeping features in training data
tst <- tst[,colnames(prj)[-ncol(prj)]]

```

1.4 Running PCA on subsets of the data
```{r}
PCA <- rbind(prj[,-ncol(prj)],tst)
# cnn pca
pca_cnn <- princomp(PCA[,which(colnames(PCA) == "cnn_9"):which(colnames(PCA) == "cnn
_89")], cor = TRUE)
summary(pca_cnn)
var <- pca_cnn[["sdev"]]^2
var_prop <- var/sum(var)
plot(cumsum(var_prop))
cnn <- pca_cnn[["scores"]][,1:7]
PCA <- PCA[,-(which(colnames(PCA) == "cnn_9"):which(colnames(PCA) == "cnn_89"))]
PCA <- cbind(cnn, PCA)
### hog pca
pca_hog <- princomp(PCA[,which(colnames(PCA) == "hog_0"):
 which(colnames(PCA) == "hog_863")], cor = TRUE)
summary(pca_hog)
var <- pca_hog[["sdev"]]^2
var_prop <- var/sum(var)
plot(cumsum(var_prop), xlab = "Number of PCs", ylab = "Cumulative Prop oF Variance")
hog <- pca_hog[["scores"]][,1:76]
PCA <- PCA[,-(which(colnames(PCA) == "hog_0"):
 which(colnames(PCA) == "hog_863"))]
PCA <- cbind(hog, PCA)
### pixel color pca
pca_pixel <- princomp(PCA[,which(colnames(PCA) == "mean_red"):
 which(colnames(PCA) == "sd_blue")], cor = TRUE)
summary(pca_pixel)
var <- pca_pixel[["sdev"]]^2
var_prop <- var/sum(var)
plot(cumsum(var_prop))
pixel <- pca_pixel[["scores"]][,1:4]
PCA <- PCA[,-(which(colnames(PCA) == "mean_red"):
 which(colnames(PCA) == "sd_blue"))]
PCA <- cbind(pixel, PCA)
# renaming principle components
colnames(PCA)[1:88] <- paste("PC", as.character(1:88),sep = "")
```
1.5 Final Data Cleaning  
```{r}
# removing/combining features with correlation > 0.9
corr <- abs(cor(PCA[,100:151]))
above <- which(corr >= 0.9, arr.ind = TRUE)
above <- above[!(above[,1] == above[,2]),]
int <- matrix(0,nrow(PCA),0)
for (i in seq(from = 1, by = 2, length.out = nrow(above)/2)) {
 int <- cbind(int,PCA[,rownames(above)[i]]*PCA[,rownames(above)[i+1]])
}
### creating mean characters per word from two highly corrleated features
int[,3] <- PCA$num_chars/PCA$num_words
colnames(int) <- paste("INT", as.character(1:ncol(int)),sep = "")
PCA <- PCA[,-which(colnames(PCA) %in% rownames(above))]
PCA <- cbind(PCA, int)
# making appropriate features into ordered factors and factors
PCA$avg_growth <- factor(PCA$avg_growth, ordered = TRUE)
PCA$sub_count <- factor(PCA$sub_count, ordered = TRUE)
PCA$view_count <- factor(PCA$view_count, ordered = TRUE)
PCA$video_count <- factor(PCA$video_count, ordered = TRUE)
PCA$month <- factor(PCA$month)
PCA$weekday <- factor(PCA$weekday)
# making an original copy
PCA_copy <- PCA

```

```{r}
# splitting back into training and test data
train_PCA <- PCA[1:nrow(prj),]
train_PCA <- cbind(train_PCA, "growth_2_6" = prj$growth_2_6)
test_PCA <- PCA[(nrow(prj)+1):nrow(PCA),]

```

## Methodology: Statistical Model

2.1 Removing features with negative % MSE value by while loop
```{r}
# setting out of bag method to evaluate
oob_train_control <- trainControl(method="oob", savePredictions = TRUE)
last_rmse <- matrix(0,0,3)
last_fac <- list()
colnames(last_rmse) <- c("caret","randomforest","average")
value <- TRUE
i <- 1
while(value){
 train_PCA <- PCA[1:nrow(prj),]
 train_PCA <- cbind(train_PCA, "growth_2_6" = prj$growth_2_6)
 rf <- train(growth_2_6 ~ . , data = train_PCA, method = "rf" ,
 ntree = 200, importance = TRUE, trControl = oob_train_control)
 rf_tree_imp <- varImp(rf, scale = FALSE)
 rf_tree_imp <- rf_tree_imp[["importance"]]
 month_ind <- which(grepl("month",rownames(rf_tree_imp)))
 if(sum(month_ind) != 0){
 month_sum <- sum(rf_tree_imp[month_ind,])
 rf_tree_imp <- rf_tree_imp[-month_ind,1, drop = FALSE]
 rf_tree_imp <- rbind(rf_tree_imp, "month" = month_sum)
 }
 weekday_ind <- which(grepl("weekday",rownames(rf_tree_imp)))
 if(sum(weekday_ind) != 0){
 weekday_sum <- sum(rf_tree_imp[weekday_ind,])
 rf_tree_imp <- rf_tree_imp[-weekday_ind,1, drop = FALSE]
 rf_tree_imp <- rbind(rf_tree_imp, "weekday" = weekday_sum)
 }
 sub_ind <- which(grepl("sub_count",rownames(rf_tree_imp)))
 if(sum(sub_ind) != 0){
 sub_sum <- sum(rf_tree_imp[sub_ind,])
 rf_tree_imp <- rf_tree_imp[-sub_ind,1, drop = FALSE]
 rf_tree_imp <- rbind(rf_tree_imp, "sub_count" = sub_sum)
 }
 view_ind <- which(grepl("view_count",rownames(rf_tree_imp)))
 if(sum(view_ind) != 0){
 view_sum <- sum(rf_tree_imp[view_ind,])
 rf_tree_imp <- rf_tree_imp[-view_ind,1, drop = FALSE]
 rf_tree_imp <- rbind(rf_tree_imp, "view_count" = view_sum)
 }
 vid_ind <- which(grepl("video_count",rownames(rf_tree_imp)))
 if(sum(vid_ind) != 0){
 vid_sum <- sum(rf_tree_imp[vid_ind,])
 rf_tree_imp <- rf_tree_imp[-vid_ind,1, drop = FALSE]
 rf_tree_imp <- rbind(rf_tree_imp, "video_count" = vid_sum)
 }
 avg_ind <- which(grepl("avg_growth",rownames(rf_tree_imp)))
 if(sum(avg_ind) != 0){
 avg_sum <- sum(rf_tree_imp[avg_ind,])
 rf_tree_imp <- rf_tree_imp[-avg_ind,1, drop = FALSE]
 rf_tree_imp <- rbind(rf_tree_imp, "avg_growth" = avg_sum)
 }
 caret_rmse <- rf[["results"]][["RMSE"]][2]
 m <- rf$bestTune[1,1]
 random_forest <- randomForest(growth_2_6 ~ . , data = train_PCA, mtry = m, ntree =
200)
 rf_rmse <- mean((random_forest$predicted - random_forest$y)^2)
 last_rmse <- rbind(last_rmse, c(caret_rmse,rf_rmse, (caret_rmse + rf_rmse)/2))
 last_fac[[i]] <- colnames(PCA)
 k <- rownames(rf_tree_imp)[which(rf_tree_imp[,1] >= 0)]
 PCA <- PCA[,k]
 i <- i+1
 value <- any(rf_tree_imp[,1] < 0)
}

```

2.2 Removing feature low negative % MSE value by while loop until 10 featurse remaining  
```{r}
last_rmse_2 <- matrix(0,0,3)
last_fac_2 <- list()
i <- 1
while(ncol(PCA) != 10){
 train_PCA <- PCA[1:nrow(prj),]
 train_PCA <- cbind(train_PCA, "growth_2_6" = prj$growth_2_6)
 rf <- train(growth_2_6 ~ . , data = train_PCA, method = "rf" ,
 ntree = 200, importance = TRUE, trControl = oob_train_control)
 rf_tree_imp <- varImp(rf, scale = FALSE)
 rf_tree_imp <- rf_tree_imp[["importance"]]
 month_ind <- which(grepl("month",rownames(rf_tree_imp)))
 if(sum(month_ind) != 0){
 month_sum <- sum(rf_tree_imp[month_ind,])
 rf_tree_imp <- rf_tree_imp[-month_ind,1, drop = FALSE]
 rf_tree_imp <- rbind(rf_tree_imp, "month" = month_sum)
 }
 weekday_ind <- which(grepl("weekday",rownames(rf_tree_imp)))
 if(sum(weekday_ind) != 0){
 weekday_sum <- sum(rf_tree_imp[weekday_ind,])
 rf_tree_imp <- rf_tree_imp[-weekday_ind,1, drop = FALSE]
 rf_tree_imp <- rbind(rf_tree_imp, "weekday" = weekday_sum)
 }
 sub_ind <- which(grepl("sub_count",rownames(rf_tree_imp)))
 if(sum(sub_ind) != 0){
 sub_sum <- sum(rf_tree_imp[sub_ind,])
 rf_tree_imp <- rf_tree_imp[-sub_ind,1, drop = FALSE]
 rf_tree_imp <- rbind(rf_tree_imp, "sub_count" = sub_sum)
 }
 view_ind <- which(grepl("view_count",rownames(rf_tree_imp)))
 if(sum(view_ind) != 0){
 view_sum <- sum(rf_tree_imp[view_ind,])
 rf_tree_imp <- rf_tree_imp[-view_ind,1, drop = FALSE]
 rf_tree_imp <- rbind(rf_tree_imp, "view_count" = view_sum)
 }
 vid_ind <- which(grepl("video_count",rownames(rf_tree_imp)))
 if(sum(vid_ind) != 0){
 vid_sum <- sum(rf_tree_imp[vid_ind,])
 rf_tree_imp <- rf_tree_imp[-vid_ind,1, drop = FALSE]
 rf_tree_imp <- rbind(rf_tree_imp, "video_count" = vid_sum)
 }
 avg_ind <- which(grepl("avg_growth",rownames(rf_tree_imp)))
 if(sum(avg_ind) != 0){
 avg_sum <- sum(rf_tree_imp[avg_ind,])
 rf_tree_imp <- rf_tree_imp[-avg_ind,1, drop = FALSE]
 rf_tree_imp <- rbind(rf_tree_imp, "avg_growth" = avg_sum)
 }
 caret_rmse <- rf[["results"]][["RMSE"]][2]
 m <- rf$bestTune[1,1]
 random_forest <- randomForest(growth_2_6 ~ . , data = train_PCA, mtry = m, ntree =
200)
 rf_rmse <- mean((random_forest$predicted - random_forest$y)^2)
 last_rmse_2 <- rbind(last_rmse_2, c(caret_rmse,rf_rmse, (caret_rmse + rf_rmse)/2))
 ordered_imp <- rf_tree_imp[order(rf_tree_imp[,1], decreasing = TRUE),1, drop = FAL
SE]
 ordered_imp <- ordered_imp[-nrow(ordered_imp),1, drop = FALSE]
 d <- rownames(ordered_imp)
 last_fac_2[[i]] <- colnames(PCA)
 i <- i + 1
 PCA <- PCA[,d]
}

```

2.3  Choosing final 27 predictors based on stored rmse values above and tuned m value
```{r}
PCA <- PCA_copy[,last_fac_2[[52]]]
train_PCA <- PCA[1:nrow(prj),]
train_PCA <- cbind(train_PCA, "growth_2_6" = prj$growth_2_6)
recommended.mtry <- 10
tunegrid <- expand.grid(mtry=recommended.mtry)
rf.1 <- train(growth_2_6 ~ . , data = train_PCA, method = "rf" ,
 ntree = 200, importance = TRUE, trControl = oob_train_control, tuneGri
d = tunegrid)

```

## Other Methods Tested

3.1 Collection of Code for models and methods not used for final models
```{r}
### Ridge Regression
# finding best lambda
i.exp <- seq(10, -5, length = 500)
grid <- 10^i.exp
cv_lambda <- cv.glmnet(model.matrix(growth_2_6~.,prj)[,-1], prj$growth_2_6,
 family = "gaussian", alpha = 0, lambda = grid,
 standardize = TRUE, nfolds = 10)
ridge_MSE <- numeric(0)
for (i in folds) {
 train <- model.matrix(growth_2_6~.,prj[-i, ])[,-1]
 train_y <- prj$growth_2_6[-i]
 lambda <- cv_lambda$lambda.1se
 ridge <- glmnet(train, train_y, family = "gaussian", alpha = 0,
 lambda = lambda, standardize = TRUE)
 test <- model.matrix(growth_2_6~.,prj[i, ])[,-1]
 test_y <- prj$growth_2_6[i]
 ridge_pred <- predict(ridge, test)

 ridge_MSE <- c(ridge_MSE, mean((test_y - ridge_pred)^2))
}
### LASSO
# finding best lambda
i.exp <- seq(10, -5, length = 500)
grid <- 10^i.exp
cv_lambda <- cv.glmnet(model.matrix(growth_2_6~.,prj)[,-1], prj$growth_2_6,
 family = "gaussian", alpha = 1, lambda = grid,
 standardize = TRUE, nfolds = 10)
lasso_MSE <- numeric(0)
for (i in folds) {
 train <- model.matrix(growth_2_6~.,prj[-i, ])[,-1]
 train_y <- prj$growth_2_6[-i]
 lambda <- cv_lambda$lambda.1se
 lasso <- glmnet(train, train_y, family = "gaussian", alpha = 1,
 lambda = lambda, standardize = TRUE)
 test <- model.matrix(growth_2_6~.,prj[i, ])[,-1]
 test_y <- prj$growth_2_6[i]
 lasso_pred <- predict(lasso, test)

 lasso_MSE <- c(lasso_MSE, mean((test_y - lasso_pred)^2))
}
# Coefficients
lasso <- glmnet(scale(model.matrix(growth_2_6~.,prj))[,-1], scale(prj$growth_2_6),
 family = "gaussian", alpha = 1,
 lambda = grid, standardize = TRUE)
i.exp <- seq(10, -5, length = 500)
grid <- 10^i.exp 
cv_lambda_lasso <- cv.glmnet(scale(model.matrix(growth_2_6~.,prj))[,-1], scale(prj$g
rowth_2_6),
 family = "gaussian", alpha = 1, lambda = grid,
 standardize = TRUE, nfolds = 10)
lasso_best <- glmnet(scale(model.matrix(growth_2_6~.,prj))[,-1], scale(prj$growth_2_
6),
 family = "gaussian", alpha = 1,
 lambda = cv_lambda_lasso$lambda.min, standardize = TRUE)
lasso_coef <- predict(lasso_best,lambda,type = "coefficients")
lasso_coef <- as.numeric(lasso_coef)[-1]
lasso_coef <- which(lasso_coef!=0) #not zero
lasso_coef <- which(abs(lasso_coef)>= 0.025) # greater than 0.025
lasso_coef <- which(abs(lasso_coef)>= 0.05) # greater than 0.05
lasso_coef <- colnames(prj)[lasso_coef]
### Elastic Net
alp <- 0.8
enet <- glmnet(scale(model.matrix(growth_2_6~.,prj))[,-1], scale(prj$growth_2_6),
 family = "gaussian", alpha = alp,
 lambda = grid, standardize = TRUE)
plot(enet, xvar = "lambda", label = TRUE)
cv_lambda_enet <- cv.glmnet(scale(model.matrix(growth_2_6~.,prj))[,-1], scale(prj$gr
owth_2_6),
 family = "gaussian", alpha = alp, lambda = grid,
 standardize = TRUE, nfolds = 10)
enet_best <- glmnet(scale(model.matrix(growth_2_6~.,prj))[,-1], scale(prj$growth_2_
6),
 family = "gaussian", alpha = alp,
 lambda = cv_lambda_enet$lambda.1se, standardize = TRUE)
enet_coef <- predict(enet_best,lambda,type = "coefficients")
enet_coef <- as.numeric(enet_coef)[-1]
enet_coef <- which(enet_coef!=0) #not zero
enet_coef <- which(abs(enet_coef)>= 0.1) # greater than 0.1
enet_coef <- which(abs(enet_coef)>= 0.05) # greater than 0.025
enet_coef <- colnames(prj)[enet_coef]
### PCR
pcr_fit <- pcr(growth_2_6~., data = prj, scale = TRUE, validation = "CV")
validationplot(pcr_fit, val.type = 'MSEP')
pcr_MSE <- numeric(0)
for (i in folds) {
 pcr_fit <- pcr(growth_2_6~., data = prj[-i,], scale = TRUE)
 pcr_pred <- predict(pcr_fit, prj[i,], ncomp=50)
 pcr_MSE <- c(pcr_MSE, mean((prj$growth_2_6[i] - pcr_pred)^2))
}
### PLS
pls_fit <- plsr(growth_2_6~., data = prj, scale = TRUE, validation = "CV")
validationplot(pls_fit, val.type = 'MSEP')
pls_MSE <- numeric(0)
for (i in folds) {
 pls_fit <- plsr(growth_2_6~., data = prj[-i,], scale = TRUE)
 pls_pred <- predict(pls_fit, prj[i,], ncomp=50)
 pls_MSE <- c(pls_MSE, mean((prj$growth_2_6[i] - pls_pred)^2))
}
### PCA then KNN
pca_ft <- cbind(pca$scores[,1:100],"growth_2_6" = prj$growth_2_6)
pca_ft <- as.data.frame(pca_ft)
knn_MSE <- numeric(0)
for (i in folds) {
 fit_knn <- knnreg(pca_ft[-i,-ncol(pca_ft)], pca_ft$growth_2_6[-i], k = 10)
 pred_knn <- predict(fit_knn, pca_ft[i,-ncol(pca_ft)])
 knn_MSE <- c(knn_MSE, mean((pca_ft$growth_2_6[i] - pred_knn)^2))
}
View(cbind(pred_knn,pca_ft$growth_2_6[i],pca_ft$growth_2_6[i] - pred_knn))
View(cbind(prj$growth_2_6[i],pca_ft$growth_2_6[i]))
### LASSO then PCA then KNN
lasso_ft <- cbind(prj[,lasso_coef], "growth_2_6" = prj$growth_2_6)
pca_lasso_min <- princomp(lasso_ft[,-ncol(lasso_ft)], cor = TRUE)
var <- pca_lasso_min[["sdev"]]^2
var_prop <- var/sum(var)
plot(cumsum(var_prop))
lasso_pca_ft <- cbind(pca_lasso_min$scores[,1:21],"growth_2_6" = prj$growth_2_6)
lasso_pca_ft <- as.data.frame(lasso_pca_ft)
knn_MSE <- numeric(0)
for (i in folds) {
 fit_knn <- knnreg(lasso_pca_ft[-i,-ncol(lasso_pca_ft)], lasso_pca_ft$growth_2_6[-
i], k = 150)
 pred_knn <- predict(fit_knn, lasso_pca_ft[i,-ncol(lasso_pca_ft)])
 knn_MSE <- c(knn_MSE, mean((lasso_pca_ft$growth_2_6[i] - pred_knn)^2))
}
### LASSO then PCR
lasso_ft <- cbind(prj[,lasso_coef], "growth_2_6" = prj$growth_2_6)
pcr_fit <- pcr(growth_2_6~., data = lasso_ft, scale = TRUE, validation = "CV")
validationplot(pcr_fit, val.type = 'MSEP')
pcr_MSE <- numeric(0)
for (i in folds) {
 pcr_fit <- pcr(growth_2_6~., data = lasso_ft[-i,], scale = TRUE)
 pcr_pred <- predict(pcr_fit, lasso_ft[i,], ncomp=150)
 pcr_MSE <- c(pcr_MSE, mean((lasso_ft$growth_2_6[i] - pcr_pred)^2))
}
View(cbind(pcr_pred, prj$growth_2_6[i]))
### LASSO then KNN
lasso_ft <- cbind(prj[,lasso_coef], "growth_2_6" = prj$growth_2_6)
lasso_ft[,-ncol(lasso_ft)] <- scale(lasso_ft[,-ncol(lasso_ft)])
lasso_knn_MSE <- numeric(0)
for (i in folds) {
 fit_knn <- knnreg(lasso_ft[-i,-ncol(lasso_ft)], lasso_ft$growth_2_6[-i], k = 200)
 pred_knn <- predict(fit_knn, lasso_ft[i,-ncol(lasso_ft)])
 lasso_knn_MSE <- c(lasso_knn_MSE, mean((lasso_ft$growth_2_6[i] - pred_knn)^2))
}
### PLS then KNN
pls_ft <- cbind(pls_fit$scores[,1:10],"growth_2_6" = prj$growth_2_6)
pls_ft <- as.data.frame(pls_ft)
knn_MSE <- numeric(0)
for (i in folds) {
 fit_knn <- knnreg(pls_ft[-i,-ncol(pls_ft)], pls_ft$growth_2_6[-i], k = 115)
 pred_knn <- predict(fit_knn, pls_ft[i,-ncol(pls_ft)])
 knn_MSE <- c(knn_MSE, mean((pls_ft$growth_2_6[i] - pred_knn)^2))
}
View(cbind(pred_knn,pls_ft$growth_2_6[i],pls_ft$growth_2_6[i] - pred_knn))
View(cbind(prj$growth_2_6[i],pls_ft$growth_2_6[i]))
### PCA then RF
pca_ft <- cbind(pca$scores[,1:150],"growth_2_6" = prj$growth_2_6)
pca_ft <- as.data.frame(pca_ft)
rf_tree_pca <- randomForest(growth_2_6~., data = pca_ft, ntree = 150, importance = T
RUE)
plot(rf_tree_pca)
# importance
varImpPlot(rf_tree_pca)
rf_tree_pca_imp <- rf_tree_pca$importance
# out of bag method
oob_train_control <- trainControl(method="oob", savePredictions = TRUE)
# rfm with best m
rf_pca <- train(growth_2_6 ~ . , data = pca_ft, method = "rf" ,
 ntree = 100, importance = FALSE, trControl = oob_train_control)
plot(rf_pca)
### LASSO then RF
lasso_ft <- cbind(prj[,lasso_coef], "growth_2_6" = prj$growth_2_6)
rf_tree_lasso <- randomForest(growth_2_6~., data = lasso_ft, ntree = 150, importance
 = TRUE)
plot(rf_tree_lasso)
# importance
varImpPlot(rf_tree_lasso)
rf_tree_lasso_imp <- rf_tree_lasso$importance
# out of bag method
oob_train_control <- trainControl(method="oob", savePredictions = TRUE)
# rfm with best m
rf_lasso <- train(growth_2_6 ~ . , data = lasso_ft, method = "rf" ,
 ntree = 200, importance = FALSE, trControl = oob_train_control)
plot(rf_lasso)
### LASSO + PCA then
lasso_ft <- cbind(prj[,lasso_coef], "growth_2_6" = prj$growth_2_6)
pca_lasso_min <- princomp(lasso_ft[,-ncol(lasso_ft)], cor = TRUE)
var <- pca_lasso_min[["sdev"]]^2
var_prop <- var/sum(var)
plot(cumsum(var_prop))
lasso_pca_ft <- cbind(pca_lasso_min$scores[,],"growth_2_6" = prj$growth_2_6)
lasso_pca_ft <- as.data.frame(lasso_pca_ft)
rf_tree_lasso_pca <- randomForest(growth_2_6~., data = lasso_pca_ft, ntree = 150, im
portance = TRUE)
plot(rf_tree_lasso_pca)
# importance
varImpPlot(rf_tree_lasso_pca)
rf_tree_lasso_pca_imp <- rf_tree_lasso_pca$importance
# out of bag method
oob_train_control <- trainControl(method="oob", savePredictions = TRUE)
# rfm with best m
rf_lasso_pca <- train(growth_2_6 ~ . , data = lasso_pca_ft, method = "rf" ,
 ntree = 200, importance = FALSE, trControl = oob_train_control)
plot(rf_lasso_pca)
```

