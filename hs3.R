library(data.table)
library(xgboost)
library(Metrics)
library(Matrix)
library(mice)
library(dplyr)
library(caret)
library(psych)
library(randomForest)
library(e1071)
setwd("C:/Users/Jyoti Prakash/Desktop/Housing Prediction")

#load data
train=read.csv("train.csv",header = TRUE)
test=read.csv("test.csv",header=TRUE)
y_train=train$SalePrice

#Remove Id since of no use
train$Id=NULL
train$SalePrice=NULL

test$Id=NULL

#Row binding train & test set for feature engineering
train_test = rbind(train, test)
ntrain=nrow(train)

features=names(train)

#convert character into integer

for(f in features){
  if(class(train_test[[f]])=="character"){
    
    train_test[[f]]=as.numeric(as.factor(train_test[[f]]))
  }
}

#One hot encoding
#numerical.data=train_test[ ,!sapply(train_test, is.factor)]
#categorical.data=train_test[ ,sapply(train_test, is.factor)]
#dummies = dummyVars(~ ., data =categorical.data )
#data.code=as.data.frame(predict(dummies,newdata=categorical.data))
#train_test=cbind(numerical.data,categorical.data)


#Missing value imputation
#Removing colums which has more than 50% data missing
mislabel=c()
labellist=names(train_test)
for (i in labellist){
  
  if (sum(is.na(train_test[i]))>=nrow(train_test)*.5)
  {
    mislabel=c(mislabel,i)}}

#Removing colums which have more than 50% data are missing
a=train_test[,-c(which(colnames(train_test) %in% mislabel))]
train_test=as.data.frame(a)

#Imputing missing values using MICE
#library(mice)
#md.pattern(train_test)

#Analyzing NA values
sum(is.na(train_test))
colSums(is.na(train_test))
sum(is.na(train_test))
#imputed_Data = mice(train_test, m=2, maxit = 10, method = 'fastpmm', seed = 500)
#completedata=complete(imputed_Data)
#colSums(is.na(train_test))


#missforest
library(missForest)
Xms <- missForest(train_test)
train_test=Xms$ximp

#PCA
prepca=preProcess(train_test,method=c("pca"),thresh=.98)
proTrain=predict(prepca,train_test)
train_test=data.frame(proTrain)

train_test[] <- lapply(train_test, as.numeric)

#Corelation matrix
cor.ci(train_test,method="spearman")







train_x=train_test[1:ntrain,]
test_x=train_test[(ntrain+1):nrow(train_test),]
train_xl=cbind(train_x,"SalePrice"=y_train)
#colnames(train_xl)[60]="SalePrice"


#Feature selection:Boruta
library(Boruta)
set.seed(123)
boruta.train =Boruta(SalePrice~., data = train_xl, doTrace = 2)
print(boruta.train)
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
impF=getSelectedAttributes(final.boruta, withTentative = F)

train_x=train_x[,names(train_x) %in% impF]
test_x=test_x[,names(test_x) %in% impF]
train_xl=cbind(train_x,"SalePrice"=y_train)

#Data spliting for train and test
library(caTools)
set.seed(12)
spl=sample.split(train_xl$SalePrice,SplitRatio=0.7)
train1=subset(train_xl,spl==TRUE)
test1=subset(train_xl,spl==FALSE)

linear=lm(SalePrice~.,data=train1)
pred=predict(linear,newdata=test1)
rmse(train1$SalePrice,pred)

#Decision Tree
library(rpart)
model1=rpart(SalePrice~.,data=train1)
pred=predict(model1,newdata=test1)
rmse(test1$SalePrice,pred)






#Randomforest
library(randomForest)
model1=randomForest(SalePrice~.,data=train1)
pred=predict(model1,newdata=test1)
rmse(test1$SalePrice,pred)






#xgboost
trainx1=subset(train1,select=-SalePrice)
testx1=subset(test1,select=-SalePrice)
trainx1[] <- lapply(train1, as.numeric)
testx1[]<-lapply(test1, as.numeric)

y_train=train1[,"SalePrice"]

dtrain=xgb.DMatrix(as.matrix(train1),label= y_train)
dtest=xgb.DMatrix(as.matrix(test_x))

#xgboost parameters
xgb_params = list(
  seed = 0,
  colsample_bytree = 0.5,
  subsample = 0.8,
  eta = 0.02, 
  objective = 'reg:linear',
  max_depth = 12,
  alpha = 1,
  gamma = 2,
  min_child_weight = 1,
  base_score = 7.76
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

best_n_rounds=150 # try more rounds

#train data
gb_dt=xgb.train(xgb_params,dtrain,nrounds = as.integer(best_n_rounds))
pred=predict(gb_dt,dtest)
rmse(test1$SalePrice,pred)



