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

#Removing colums which has more than 50% data missing
mislabel=c()
labellist=names(full.data)
for (i in labellist){

 if (sum(is.na(full.data[i]))>=nrow(full.data)*.5)
  {
    mislabel=c(mislabel,i)}}

#Removing colums which have more than 50% data are missing
full.data=full.data[,-c(which(colnames(full.data) %in% mislabel))]
l=as.data.frame()
miscol=(names(full.data[colSums(is.na(full.data))>0]))
misval=colSums(is.na(full.data[c(colnames(full.data) %in% miscol)]))
miscol=is.data.frame(miscol)
miscol$value=misval

#Imputing missing values
full.data[is.na(full.data$MSZoning),]
full.data[is.na(full.data$MSZoning),"MSZoning"]='RL' #Imputing with mode
full.data[is.na(full.data$Utilities),"Utilities"]='AllPub' #Imputing with mode
full.data[is.na(full.data$Exterior1st),"Exterior1st"]='VinylSd' #Imputing with mode
full.data[is.na(full.data$Exterior2nd),"Exterior2nd"]='VinylSd' #Imputing with mode
full.data[is.na(full.data$BsmtFinSF1),"BsmtFinSF1"]=368.5 #Imputing with median
smallmiss=c("BsmtFinSF2", 'BsmtUnfSF',  'TotalBsmtSF','Electrical', 'BsmtFullBath',
            'BsmtHalfBath','KitchenQual', 'Functional','GarageCars','GarageArea','SaleType')

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
for (i in smallmiss){
  {
full.data[is.na(full.data[i]),i]=mode(as.factor(full.data$i))}} #Imputing with mode


