# Loading data sets
setwd("C:/Users/Jyoti Prakash/Desktop/Housing Prediction")
train=read.csv("train.csv",header = TRUE)
test=read.csv("test.csv",header=TRUE)

#Analysing data
str(train)
str(test)

#Add SalePrice to test 
test$SalePrice=NA

#Bind in a single dataframe
full.data=rbind(train,test)

#Data Cleaning
#Analyzing NA values
sum(is.na(full.data))
colSums(is.na(full.data))

#Segregating categorical and numerical data types
numerical.data=full.data[ ,!sapply(full.data, is.factor)]
categorical.data=full.data[ ,sapply(full.data, is.factor)]

#One hot coding for categorical data
library(caret)
dummies = dummyVars(~ ., data = categorical.data)
data.code=as.data.frame(predict(dummies,newdata=categorical.data))

#Binding with numerical data
full.data=cbind(numerical.data,data.code)
y="SalePrice"
X_full.data=subset(full.data, select=-SalePrice)
                      
#Removing colums which has more than 50% data missing
mislabel=c()
labellist=names(X_full.data)
for (i in labellist){
  
  if (sum(is.na(X_full.data[i]))>=nrow(X_full.data)*.5)
  {
    mislabel=c(mislabel,i)}}

#Removing colums which have more than 50% data are missing
a=X_full.data[,-c(which(colnames(X_full.data) %in% mislabel))]
X_full.data=as.data.frame(a)

#Imputing missing values using MICE
#library(mice)
#md.pattern(X_full.data)
#library(VIM)
#mice_plot =aggr(X_full.data, col=c('navyblue','yellow'),
                  #  numbers=TRUE, sortVars=TRUE,
                   # labels=names(X_full.data), cex.axis=.7,
                  #  gap=3, ylab=c("Missing data","Pattern"))

#imputed_Data = mice(X_full.data, m=1, maxit = 10, method = 'fastpmm', seed = 500)
#completedata=complete(imputed_Data)
#completedata=na.omit(completedata)

#missforest
library(missForest)
Xms <- missForest(X_full.data)
completedata=Xms$ximp


#Train test split
x_train=completedata[1:1460,]
y_train=as.numeric(train[,'SalePrice'])

train_f$SalePrice=y_train

#70-30 split
train_f1=train_f[sample(nrow(train_f),nrow(train_f)*.7),]
test_f1=train_f[-sample(nrow(train_f),nrow(train_f)*.7),]

#Linear regression model
library(randomForest)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

#linear=lm(SalePrice~.-Id,data=train_f1)

#xgboost
#xgb <- xgboost(data = data.matrix(subset(train_f1,select=-(SalePrice))), 
 ##             eta = 0.1,
   #            max_depth = 15, 
    #           nround=25, 
     #          subsample = 0.5,
      ##        seed = 1,
        #       eval_metric = "merror",
         #      objective = "multi:softprob",
          #     num_class = 12,
           #    nthread = 3
#)
rand=randomForest(SalePrice~.,data=train_f1)
