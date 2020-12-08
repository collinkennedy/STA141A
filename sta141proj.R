#Project STA141A

rm(list = ls())
library(tidyverse)
library(readr)
library(kableExtra)
insurance <- read_csv("/Users/collinkennedy/Downloads/Downloads/insurance.csv")


#3.Can we accurately predict medical insurance costs based on our 6 attributes 
#(Age, Sex, BMI,Children, Smoker, Region)?
attach(insurance)


insuranceModel = lm(charges ~ ., data = insurance)
summary(insuranceModel)

#first, data visualization:

agePlot = ggplot(data= insurance, mapping = aes(x = age, y = charges))+
  geom_point(mapping = aes(color = smoker)) +
  scale_y_continuous(name = "Medical Charges")+
  ggtitle("Age vs Medical Charges, by smoking")
agePlot


bmiPlot = ggplot(data = insurance, mapping = aes(x = bmi, y = charges))+
  geom_point(mapping = aes(color = smoker))+
  scale_y_continuous(name = "Medical Charges")+
  ggtitle("Body Mass Index (BMI) vs Medical Charges")
bmiPlot
  

sexPlot= ggplot(data = insurance, mapping = aes(x = sex, y = charges))+
  geom_boxplot(fill = "chartreuse4", colour = "chartreuse3")+
  scale_y_continuous(name = "Medical Charges")+
  ggtitle("Boxplot: Medical Charges by Sex")
sexPlot

smokePlot = ggplot(data = insurance, mapping = aes(x = smoker, y = charges))+
  geom_boxplot(fill = "cadetblue3", colour = "cadetblue4")+
  scale_y_continuous(name = "Medical Charges")+
  ggtitle("Boxplot: Medical Charges by Smoker Status")
smokePlot

childPlot = ggplot(data = insurance, mapping = aes(x = factor(children), y = charges))+
  geom_boxplot(fill = "darkorchid3", colour = "darkorchid4")+
  scale_y_continuous(name = "Medical Charges")+
  scale_x_discrete(name="No. of Children")+
  ggtitle("Boxplot: Medical Charges by # of Children")
childPlot

regionPlot = ggplot(data = insurance, mapping = aes(x = region, y = charges))+
  geom_boxplot(fill = "darkred", colour = "firebrick")+
  scale_y_continuous(name="Medical Charges")+
  scale_x_discrete(name = "Region")+
  ggtitle("Boxplot: Medical Charges by Region")
regionPlot

library("gridExtra")
grid.arrange(agePlot, bmiPlot,sexPlot,
             smokePlot,childPlot,regionPlot,
             ncol = 2, nrow = 3)


#Determine the best model by using Backward selection
library(car)
linearHypothesis(insuranceModel, c("bmi = 0"))
AIC(insuranceModel)



plot(insuranceModel)
#residuals vs fitted values:
#Based on the residuals vs fitted values and scale location plot, there appears to be a 
#a violation of the constant variance assumption. Notice that residuals are concentrated
#in certain regions more so than others; they do not appear to be randomly scattered 
#around the graph. heteroskedasticity harms our standard error estimations. In other words,
#it harms the precision of the model, as the standard errors may be biased


#looking at the normal QQ plot, again, it is clear that the homoskedasticity assumption
#is violated. Notice how the residuals in the upper right region of the graph depart from 
#the theoretical line of normality. 

#investigating this further by looking at a histogram of the residuals of the (full)
#model, it is clear that the residuals are right skewed- they are not normally 
#distributed.


shapiro.test(insuranceModel$residuals)
#Given a p value of < 2.2e-16, we reject the null hypothesis that the residuals are 
#normally distributed in favor of the alternative hypothesis, and conclude that
#they do not follow a random distribution


#based on the diagnostics, we feel it is necessary to transform the data to 
#meet the assumptions of linear regression
library(MASS)

transformedIM = lm(log(charges) ~ log(age) + log(bmi) + children + smoker + region, data = insurance)
plot(transformedIM)







library(olsrr)  #use functions from this package to find the best 

#gg <- ols_step_all_possible(insuranceModel)

#View(gg)

#ols_step_best_subset(insuranceModel)





step(insuranceModel, direction = "backward")
#using the step function to implement backward model selection, we used AIC as our 
#criteria for choosing the best model (the model that minimizes AIC)

#we find that the best model includes all of the predictors except sex, which can be 
#dropped




#what we still need to do for this problem:
#- find an appropriate transformation to fix heteroskedasticity

#- backward selection using the step() function

#-evaluate interaction terms 








#4========================================================================================
#create training dataset and test data set
set.seed(1)
insurance$smoker = factor(insurance$smoker)


#create the training dataset
insurance_train = insurance %>% 
  sample_frac(.8)


#create the test training set
insurance_test = insurance %>% 
  sample_frac(.2)

#build the logistic regression model using the training dataset
logreg = glm(smoker~charges, data = insurance_train, family = "binomial")

summary(logreg)

# probabilities and predicted classes
prob<-predict(logreg, type = "response", newdata = insurance_test)
predicted<-ifelse(predict(logreg, type = "response",newdata = insurance_test)>.5,"yes","no")





#present accuracy rate and prediction rate


#predicted dataframe

predicted_df = data.frame(x=insurance_test$charges, y = predicted)
predicted_df
colnames(predicted_df) = c("charges", "predicted_smoker")
predicted_df

cbind(insurance_test, prob)#add the predicted probabilities to the test dataframe

# some plots

ggplot(data = insurance_test) +
  geom_point(mapping = aes (x = 1:268, y = prob, colour = smoker)) +
  geom_hline(yintercept = .5, colour = "red")+
  labs(y = "Pr(Smoker = \"Yes\" | Medical Charges)")+
  ggtitle("Scatterplot: Probability of an Individual Being a Smoker, Given Medical Charges")

#Consider this scatterplot of the probability of being a smoker given one's 
#medical. Each point represents an individual (observation) from the test insurance
#data. Points that fall above the line are classified as smokers, and the individuals
#who fall below the line are classified as non-smokers. 

#Most noticeably, the points(individuals) are color-coded based on whether or not 
#they are actually a smoker. In other words, the red points above the line, and the 
#blue points below the line represent individuals who are misclassified as smokers and non-smokers
#Incorrectly. However, it is pretty remarkeable how accurate the logistic regression 
#model appears to be with just a single predictor.
#It does appear we can predict whether or not an individual is a smoker based on their
#medical costs.

#While the visual representation is nice, let's consider a handful of useful statistics
#that allow us to quantify how useful this model is:


confusion<-table(predicted,insurance_test$smoker,dnn = c("Predicted smoker","True smoker"))

confusion = matrix(confusion,2,2)
rownames(confusion) = c("predicted no", "predicted yes")
colnames(confusion) = c("true no", "true yes")


confusion %>% 
  kbl(caption = "Contingency Table: Predicted Smoker vs. Actual Smoker") %>%
  kable_styling()


accuracy <- sum(diag(confusion))/sum(confusion)
accuracy

misclassification <- 1-accuracy
misclassification



tpr = (confusion[4])/(confusion[4] + confusion[3]) #true positive rate (same as power)
tpr

specificity = confusion[1]/(confusion[1] + confusion[2]) #true negative rate =>
#TrueNegative/(TN + FP)
specificity
#make a nice contigency table



#interpret the results, and describe the implications of the accuracy and
#misclassification rate


