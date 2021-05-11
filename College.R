library(ISLR)
library(glmnet)
library(pls)
View(College)
college<-College
colSums(is.na(college))

set.seed(14)


#(a) Dividing the Data into training and testing sets
train_rows<-sample(1:nrow(college),.75*nrow(college))
train<-college[train_rows,]
test<-college[-train_rows,]
x.train<-train[,-2]
y.train<-train[,2]
x.test<-test[,-2]
y.test<-test[,2]


#(b) Fitting the Model using Linear Regression
lm.fit=lm(Apps~.,data=data.frame(train))
summary(lm.fit)

lm.predict=predict(lm.fit,newdata=x.test)

mean((lm.predict-y.test)^2)
#1465155

#Calculating R-squared value of testing data
rss.lm<-sum((lm.predict-y.test)^2)
tss.lm<-sum((y.test-mean(y.test))^2)
rsq.lm<-1-rss.lm/tss.lm
rsq.lm
#0.8979556


#(c) Fitting the Model using Ridge Regression 
train.matrix=model.matrix(Apps~.,data=train)
test.matrix=model.matrix(Apps~.,data=test)
x.train.matrix=train.matrix[,-2]
x.test.matrix=test.matrix[,-2]

Ridge.fit<-cv.glmnet(as.matrix(x.train.matrix),y.train, type.measure="mse", 
                       alpha=0, family="gaussian")

best_lam=Ridge.fit$lambda.min

Ridge.predict<-predict(Ridge.fit,s=best_lam,newx=as.matrix(x.test.matrix))

mean((Ridge.predict-y.test)^2)
#1372154

#Calculating R-squared value of testing data
rss.Ridge<-sum((Ridge.predict-y.test)^2)
tss.Ridge<-sum((y.test-mean(y.test))^2)
rsq.Ridge<-1-rss.Ridge/tss.Ridge
rsq.Ridge
#0.9044329


#(e) Fitting Model using PCR Regression
pcr.fit=pcr(Apps~.,data=train,scale=TRUE,validation="CV")

summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")

#After viewing plot and summary of pcr.fit we found that lowest cross-validation error occurs when M=9
pcr.predict=predict(pcr.fit,x.test,ncomp=9)

mean((pcr.predict-y.test)^2)
#1692121

#Calculating R-squared value of testing data
rss.pcr<-sum((pcr.predict-y.test)^2)
tss.pcr<-sum((y.test-mean(y.test))^2)
rsq.pcr<-1-rss.pcr/tss.pcr
rsq.pcr
#0.8821479


#(g) The Errors obtained on testing data are:-
#    Using Linear Regression:- 1465155
#    Using Ridge Regression:-  1372154
#    Using PCR Regression:-    1692121
# The accuracy for this models ranges from 88%-90%.
# From this values we can see that for this College dataset Ridge Regression performs slightly better as compared to Linear and PCR Regression.
# But this difference between error values is less as the range of accuracy for this model is low.
