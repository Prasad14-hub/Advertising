#Importing Required libraries
library(ISLR)
library(glmnet)
library(pls)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tidyverse)
library(caret)
library(ggplot2)
require(plyr)
require(reshape2)
require(ggiraph)
require(rgl)
require(ggiraphExtra)
require(TH.data)
require(predict3d)
library(car)

data<-read.csv(file.choose())
View(data)

#Checking if NA values are present in dataset 
colSums(is.na(data))

set.seed(14)

#Remove/drop Unnecessary features
data$X<-NULL
View(data)

#Finding Relation between label the features
correlationMatrix<-cor(data)
print(correlationMatrix)


#Selection of features for best model on the basis of R-squared,BIC,Cp,AIC values.
fix(data)
library(leaps)
regfit.full=regsubsets(Sales~.,data)
summary(regfit.full)
regfit.full=regsubsets(Sales~.,data,nvmax = 3)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq


#Plots between RSS-ncomp,R-squared-ncomp
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")


which.max(reg.summary$adjr2)
#2

points(2,reg.summary$adjr2[2],col="red",cex=2,pch=20)


#Plot between Cp and ncomp
plot(reg.summary$cp,xlab="Number of Variables ",ylab="Cp",
     type="l")
which.min(reg.summary$cp)
#2

points(2,reg.summary$cp[2], col ="red",cex=2,pch =20)
which.min(reg.summary$bic)
#2


#plot between BIC and ncomp
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",
     type="l")
points(2,reg.summary$bic[2],col="red",cex=2,pch =20)


plot(regfit.full ,scale="r2")
plot(regfit.full ,scale="adjr2")
plot(regfit.full ,scale="Cp")
plot(regfit.full ,scale="bic")

coef(regfit.full,2)


#Forward and Backward Stepwise Selection
regfit.fwd=regsubsets(Sales∼.,data=data, nvmax=3,
                      method ="forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(Sales∼.,data=data, nvmax=3,
                      method ="backward")
summary(regfit.bwd)


#Dividing the Data into training and testing sets
train_rows<-sample(1:nrow(data),.75*nrow(data))
train<-data[train_rows,]
test<-data[-train_rows,]
x.train<-train[,-4]
y.train<-train[,4]
x.test<-test[,-4]
y.test<-test[,4]

regfit.best=regsubsets(Sales∼.,data=train,
                       nvmax=3)

test.mat=model.matrix(Sales∼.,data=test)

val.errors =rep(NA ,3)
for(i in 1:3){
  coefi=coef(regfit.best ,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((test$Sales-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)


#Making our own function for predictions
predict.regsubsets =function(object , newdata ,id ,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata )
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Sales∼.,data=data,nvmax=3)
coef(regfit.best,3)


#10 fold Cross Validation
k=10
folds=sample(1:k,nrow(data),replace=TRUE)
cv.errors=matrix(NA,k,3,dimnames =list(NULL,paste(1:3)))

for(j in 1:k){
  best.fit=regsubsets(Sales∼.,data=data[folds!=j,],
                      nvmax=3)
  for(i in 1:3){
    pred=predict(best.fit,data[folds==j,],id=i)
    cv.errors[j,i]= mean((data$Sales[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type="b")
min(mean.cv.errors)

#After all the results we found that the when the number of features are 2 we get the best fitting Model
coef(reg.best,2)




#--------------Linear Regression Model--------------




#Making new dataframes containing the 2 best Variables
train.new=train[,-3]
test.new=test[,-3]
x.train.new=train.new[,-4]
x.test.new=test.new[,-4]
  
#Fitting Model using Linear Regression
lm.fit=lm(Sales~.,data=train.new)
summary(lm.fit)

#a linear regression fit to sales using TV and radio as predictors.
scatter3d(x=train$Radio,y=train$Sales,z=train$TV)


#Simple regression of sales on TV
tab_model(lm.fit,terms = c("(Intercept)","TV"))

#Simple regression of sales on Radio 
tab_model(lm.fit,terms = c("(Intercept)","Radio"))

#Anova table 
lm_anova=aov(lm.fit)
summary(lm_anova)

#Multiple Linear Regression
multiple.fit=lm(Sales~.,data=train)
summary(multiple.fit)

#3D Plot of all the variables
predict3d(multiple.fit,radius=0.5)
rglwidget(elementId = "1st")
rgl.bringtotop()
rglwidget(elementId ="3rd")

#Multiple Linear regression of sales on all 3 variables
tab_model(multiple.fit)

#Anova table 
multiple_anova=aov(multiple.fit)
summary(multiple_anova)

#Calculating VIF scores
car::vif(multiple.fit)
#As we can see all the VIF scores are close to 1 that means there is a very small collinearity between the variables.

#Predicting Sales on testing dataset
lm.predict=predict(lm.fit,newdata=x.test.new)

#Mean squared error of testing data
mean((lm.predit-y.test)^2)
#2.595608

#Calculating R-squared value of testing data
rss.lm<-sum((lm.predict-y.test)^2)
tss.lm<-sum((y.test-mean(y.test))^2)
rsq.lm<-1-rss.lm/tss.lm
rsq.lm
#0.9095023





#--------------Ridge Regression Model--------------




#Converting training and testing dataframes into matrices
train.matrix=model.matrix(Sales~.,data=train)
test.matrix=model.matrix(Sales~.,data=test)
x.train.matrix=train.matrix[,-4]
x.test.matrix=test.matrix[,-4]

#Fitting the Model using Ridge Regression 
Ridge.fit<-cv.glmnet(as.matrix(x.train.matrix),y.train, type.measure="mse", 
                     alpha=0, family="gaussian")

best_lam=Ridge.fit$lambda.min

#Predicting Sales on testing dataset
Ridge.predict<-predict(Ridge.fit,s=best_lam,newx=as.matrix(x.test.matrix))

#Mean squared error of testing data
mean((Ridge.predict-y.test)^2)
#2.707439

#Calculating R-squared value of testing data
rss.Ridge<-sum((Ridge.predict-y.test)^2)
tss.Ridge<-sum((y.test-mean(y.test))^2)
rsq.Ridge<-1-rss.Ridge/tss.Ridge
rsq.Ridge
#0.9056102




#--------------PCR Regression Model--------------




#Fitting the Model using PCR Regression
pcr.fit=pcr(Sales~.,data=train,scale=TRUE,validation="CV")

summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")

#After viewing plot and summary of pcr.fit we found that lowest cross-validation error occurs when ncomp=3
pcr.predict=predict(pcr.fit,x.test,ncomp=3)

#Mean squared error of testing data
mean((pcr.predict-y.test)^2)
#2.595608

#Calculating R-squared value of testing data
rss.pcr<-sum((pcr.predict-y.test)^2)
tss.pcr<-sum((y.test-mean(y.test))^2)
rsq.pcr<-1-rss.pcr/tss.pcr
rsq.pcr
#0.909509

#Fitting the Model using PCA Regression
pca<-prcomp(x.train,center=TRUE,scale=TRUE)
summary(pca)
attributes(pca)


eigenvalues=pca$sdev^2
eigenvectors=pca$rotation


# make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)


#Graph for finding Percentage of Variance Each Principal Accounts for
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
pca.data


#Graph between 1st and 2nd PCs
ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")


#Creating Dataframes containing PCs
trg=predict(pca,x.train)
trg=data.frame(trg,y.train)
tst=data.frame(predict(pca,x.test))


fmla1=y.train~PC1+PC2+PC3

#Fitting Model on the training dataset
pca_model=lm(fmla1,data=data.frame(trg))


#Prediction on the testing Data
pca_pred=predict(pca_model,newdata = data.frame(tst))


#Calculating R-squared value
rss_pca <- sum((pca_pred- y.test) ^ 2)
tss_pca <- sum((y.test - mean(y.test)) ^ 2)
rsq_pca <- 1 - rss_pca/tss_pca
rsq_pca
#0.909509


#After comparing R-squared values for all the 3 Regression Models
#0.9095023 for Linear Regression
#0.9056102 for Ridge Regression
#0.909509 for PCA Regression
#Therefore for this College Dataset, PCA and Linear Regression Models slightly gives better Results/Predictions as compared to Ridge Regression Model.

