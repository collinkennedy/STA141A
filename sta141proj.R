#Project STA141A

rm(list = ls())
library(tidyverse)


#1
#3.How do smoking and BMI affect a person’s medical charges?
attach(insurance)


insuranceModel = lm(charges ~ ., data = insurance)
summary(insuranceModel)

#first, data visualization:
par(mfrow = c(3,3))
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


