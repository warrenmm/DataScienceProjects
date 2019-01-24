### Project 5A

setwd("C:/Users/Warren M/Desktop/Data Science/Week5/Proj5")
library(readxl)
library(caret)
blood=read_xlsx("blood_traindata.xlsx")
str(blood)
colnames(blood)=c("id","months_since_last","total_donations","total_ccs","months_since_first","donated")
blood$donated=factor(blood$donated)
fullset <- createDataPartition(blood$donated, p=0.80, list=FALSE)
trainset <- blood[fullset,]
testset <- blood[-fullset,]
bloodglm1= glm(donated~.+log(total_donations)-id-total_ccs, data=trainset, family="binomial")
logistic_prediction1=predict(bloodglm1,testset, data=blood, type="response")

#CrossValidation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
fit.glm <- train(donated~., data=trainset, method="glm", trControl=control)
fit.lda <- train(donated~.-id, data=trainset, method="lda", metric=metric, trControl=control)
fit.cart <- train(donated~., data=trainset, method="rpart", metric=metric, trControl=control)
fit.knn <- train(donated~., data=trainset, method="knn", metric=metric, trControl=control)
fit.svm <- train(donated~., data=trainset, method="svmRadial", metric=metric, trControl=control)
fit.rf <- train(donated~., data=trainset, method="rf", metric=metric, trControl=control)
results <- resamples(list(glm=fit.glm, lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

#CART is best. Now we test its accuracy on the train data
library(MASS)
library(tree)
library(ISLR)
tm1=tree(donated~.+log(total_donations)-total_ccs-id,trainset)
summary(tm1)
plot(tm1)
tree..pred=predict(tm1,testset)
treetest=1
treepred=matrix(1,nrow(tree..pred))
treetest=testset$donated
for (i in 1:nrow(tree..pred)){
  treepred[i,1]=round(tree..pred[i,2])
}
treepred=factor(treepred)
confusionMatrix(table(treepred,treetest))
str(trainset)
#>75% accuracy consistently. Now we apply our model to the test set
tm2=tree(donated~.-total_ccs-id, data=blood)
bloodtest=read_xlsx("blood_testdata.xlsx")
colnames(bloodtest)=c("id","months_since_last","total_donations","total_ccs","months_since_first","prediction","logistic_likelihood")

bloodglm2= glm(donated~.+log(total_donations)-id-total_ccs, data=blood, family="binomial")
logistic_prediction=predict(bloodglm2,bloodtest, data=blood, type="response")


tp2=predict(tm2,bloodtest)
tp1=matrix(2,nrow(tp2))
for (i in 1:nrow(tp2)){
  bloodtest[i,6]=round(tp2[i,2])
  bloodtest[i,7]=logistic_prediction[i]
}
bloodtest
write.csv(bloodtest,file="bloodpredictions.csv")
