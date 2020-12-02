library(tidyverse)
library(ggplot2)
library(MASS)
rm(list =ls())




#1
newIris <- iris %>% 
  filter(Species!='setosa')%>% 
  mutate(Species=factor(Species)) 


test<- newIris %>% group_by(Species) %>% slice(1:10) 
train <- newIris %>% group_by(Species) %>% slice(11:50) 
View(train)


# versicolor <- 1; virginica <-0
#test$Species<-ifelse(test$Species=="versicolor",1,0)
test$Species

#train$Species=ifelse(train$Species == "versicolor",1,0)
  
#now i need training data

#2
#**2.** *Use LDA for classifying the test data. Use Sepal.Length and Sepal.Width as the 
#predictor variables (or features) (1.5 points).*
ldaFit <- lda(Species~ Sepal.Length + Sepal.Width,data = train)



#**2a.** *Report the class-specific means of the predictor variables for the training data (1.5 points).*
ldaFit$means


#**2b.** *Compute the confusion matrix for the test data and the misclassification error 
#rate (1.5 points).*
# confusion matrix
irisPred <- predict(ldaFit, newdata=test) #use the test data in the model that was trained
                                            #using a different dataset
irisPred$class

confusionMatrix <- table(irisPred$class,test$Species,dnn = c('Predicted Species','Species'))
#note the arguments in the table function:
#first argument is the predicted classes from the predict() output
#second argument 
knitr::kable(confusionMatrix)
confusionMatrix

accLDA = sum(diag(confusionMatrix))/length(test$Species)
class(accLDA)
misclassificationRateLDA = 1 - accLDA
misclassificationRateLDA


ldaVec = c(accLDA,misclassificationRateLDA)
ldaVec
#3-----------------------------------------------------------------------------------------
#logistic regression
#The logistic regression coefficients give the change in the log odds of the outcome 
#for a one unit increase in the predictor variable.

logisticFit = glm(Species ~ Sepal.Length + Sepal.Width, data = train, family = binomial)
#use the train dataset to build the model


#i Obtain the estimates and their standard errors for the model parameters
summary(logisticFit) #estimates of the parameters and std. errors are seen in the table

#ii Compute the confusion matrix for the test data and the misclassification error rate 

# predicted classes
predictedProbs<-predict(logisticFit, type = "response",newdata = test)
predictedProbs
test


predicted<-ifelse(predict(logisticFit, type = "response",newdata = test)>.5,"versicolor","virginica")
predicted


#append the test dataset with the predicted values

confMat = table(predicted,test$Species,dnn = c("Predicted Species", "True Species") )
confMat


#misclassification rate = 1 - accuracy
#accuracy
acc = sum(diag(confMat))/length(predicted)
acc
misclassificationRate = 1 - acc
misclassificationRate

logistic2PVec = c(acc,misclassificationRate)
logistic2PVec


#iii
#Are both of the predictor variables necessary for the purpose of classification? 
summary(logisticFit)
#no, sepal.width is not statistically significant. 




#b)Fit a logistic regression model to the training data, using the variable 
#Sepal.Length as a one-dimensional predictor (3 points).
logFit2 = glm(Species ~ Sepal.Length, data = train, family = binomial)
#i
summary(logFit2)

length(test$Sepal.Length)
length(train$Sepal.Length)

#ii Compute the confusion matrix for the test data, and the misclassification error rate (1.5 points).
predicted2<-ifelse(predict(logFit2, type = "response",newdata = test)>.5,"versicolor","virginica")
confusMat = table(predicted2,test$Species,dnn = c("Predicted Species", "True Species") )
confusMat

acc2 = sum(diag(confusMat))/length(predicted2)
acc2
misclassificationRate2 = 1 - acc2
misclassificationRate2

logistic1PVec = c(acc2,misclassificationRate2)
logistic1PVec





#iii
#yes, my findings support my answer to 3aiii. Notice the misclassification rights are the 
#same, and the Residual Deviance is about the same as well.


#4Use the k-nearest neighbors (k-NN) classification method to classify the test data, 
#using only Sepal.Length as the predictor variable.Perform this analysis using k=1 and k=5. In each case, compute the confusion matrix for the test data, and the misclassification error rate (3 points).



#train, the predictors for the train set.
#test, the predictors for the test set. knn() will output results (classifications) for these cases.
#cl, the true class labels for the train set.
#k, the number of neighbors to consider.

library(class)
?knn
View(test)

trainSepalLength = train[1:80,1]
trainSepalLength
testSepalLength = test[1:20,1]
trueClassifications = train[1:80,5]
trueClassifications

train=as_tibble(train)
SLTrain = train %>% dplyr::select(Sepal.Length, Species)
dim(SLTrain)

SLTest = test %>% 
  dplyr::select(Sepal.Length,Species)

SLTest


#train and test arguments must be dataframes, but the cl argument (the true class levels of the training data)
#must be a factor vector
knnPredicted1 = knn(train = SLTrain[,1,drop = FALSE],test = SLTest[,1,drop=FALSE], cl = SLTrain$Species, k =1)
knnPredicted1

knnPredicted5 = knn(train = SLTrain[,1,drop = FALSE],test = SLTest[,1,drop=FALSE], cl = SLTrain$Species, k =5)
knnPredicted5


test = test %>% add_column(knnPredicted5)
View(test)

#for k = 1
confusionMatrixK1 = table(knnPredicted1,test$Species,dnn = c("Predicted Species, k = 1", "Actual"))
confusionMatrixK1


accuracy1 = sum(diag(confusionMatrixK1))/length(knnPredicted1)
accuracy1
mcr1= 1 - accuracy1
mcr1

kNN1Vec = c(accuracy1,mcr1)
kNN1Vec

#for k = 5
confusionMatrixK5 = table(knnPredicted5,test$Species,dnn = c("Predicted Species, k = 5", "Actual"))
confusionMatrixK5

accuracy2 = sum(diag(confusionMatrixK5))/length(knnPredicted5)
accuracy2
mcr2= 1 - accuracy2
mcr2

kNN5Vec = c(accuracy2,mcr2)
kNN5Vec



#5 Summary comparing their performances
ldaVec
logistic1PVec
logistic2PVec
kNN1Vec
kNN5Vec


summaryDF = data.frame(LDA = ldaVec,logistic_2_predictor = logistic2PVec,
                       logistic_1_predictor = logistic1PVec,kNN_k_equals_1 = kNN1Vec,
                       kNN_k_equals_5 = kNN5Vec)
View(summaryDF)

rownames(summaryDF) = c("accuracy rate","misclassification rate")

View(summaryDF)

#Here is a dataframe that summarizes the effectiveness of these 5 different models.
#the Linear Discriminant Model and kNN model (where k = 5) performed similarly in that
#they both had the same accuracy and misclassification rates. 

#Speaking of the kNN model (where k = 5), notice how it actually performed worse than
#the kNN model where k = 1. This is good evidence as to why a higher k isn't always better,
#even though intuitively at first it may seem like a larger k would be better. The kNN model
#where k = 1, actually was the most accurate model, and also had the lowest misclassification rate.

#Both logistic models (with one and 2 predictors) performed the same in terms of accuracy 
#and their misclassification rates.

#















