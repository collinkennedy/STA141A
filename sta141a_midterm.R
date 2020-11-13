#STA141A
#Collin Kennedy

library(dplyr)
library(ggplot2)
View(msleep)

#Data Exploration and Visualization---------------------------------------------------------
#a)
#subset msleep to not include missing values of vore

newData = msleep %>% 
  filter(vore != "NA")
View(newData)

#b)
#which 5 animals sleep for the shortest amount of time?
shortestSleep = newData %>% 
  arrange(sleep_total)
print(shortestSleep,n=5)
View(shortestSleep)

#the list of five animals that sleep the shortest amount of time in the dataset, from least
#to greatest is the giraffe (1.9), pilot whale (2.7),Horse (2.9), Roe deer (3.0), and Donkey (3.1)


#c)
groupedByVore = newData %>% 
  group_by(vore) %>% 
  summarize(meanSleep= mean(sleep_total)) %>%
  arrange(desc(meanSleep))

print(groupedByVore,n=4)

#insectivores spend the most time sleeping, at about 14.9 hours on average. Omnivores are next,
#and spend about 10.9 hours sleeping on average, while carnivores and herbivores sleep for
#10.4 and 9.51 hours on average.

#d)
#scatterplot

scatter = ggplot(data = newData) +
  geom_point(mapping = aes(x = awake, y = bodywt))+
  labs(title = "Scatterplot: Bodyweight vs. (hours) Awake")

scatter

#e) 
#now make points colored based on value of vore
scatter2 = ggplot(data = newData) +
  geom_point(mapping = aes(x = awake, y = bodywt, colour = vore))+
  labs(title = "Scatterplot: Bodyweight vs. (hours) Awake")

scatter2
 

#f) 
#only include herbivores that weigh 10 or less
scatter3 = ggplot(data = newData, mapping = aes(x = awake, y = bodywt,colour= vore)) +
  geom_point(data = dplyr::filter(newData, vore == "herbi",bodywt <= 10))+
  labs(title = "Scatterplot: Bodyweight vs. (hours) Awake",
       subtitle = "herbivores that weight 10 kilograms or less")
scatter3



#Multiple Linear Regression
library(MASS)
UScereal


View(UScereal)
attach(UScereal)
#a)
reg = lm(calories ~ protein + fat + fibre + carbo + sugars, data = UScereal)
summary(reg)

par(mfrow=c(1,1))
plot(reg)

#b) Yeah, the fact that fiber is not statistically significant makes sense.
#I'm no nutritionist, but I do know fiber provides very little nutritional sustenance
#since it is not digested, and therefore doesn't get stored or used as energy.

#c)
#1 The statistical significance of coefficients we know contribute to caloric intake 
#(protein, fat, carbo and sugars), and fibre's lack of statistical significance
# is a good indicator of a good model. On this same note, the F statistic of 613.1 tells us
#this model demonstrates overall high significance and is a good fit.

#2Given an  adjusted R^2 of about 98%, this model explains much of the variability in Y 
#(calories), while also taking into consideration the number of variables included in the 
#model


#3) The QQplot doesn't look to concerning. There are undoubtedly a handful of outliers,
#but the majority of the residuals fall along the theoretical line of normality, suggesting
#that the errors are approximately normally distributed. Looking at the histogram of 
#residuals, my hunch of approximate normality is confirmed
hist(reg$residuals)

#d)

#1 Based on the residuals vs. fitted values plot, the relationship between our dependent
#variable and regressors may be non-linear (note the parabolic shape of the red line). This
#of course may be a result of outliers

#2 On the same note, there appear to be a handful of problematic and highly influential 
#outliers in the dataset that are causing problems for the fit of the model. This is 
#noticeable in the Normal QQ plot, as well as the Residuals vs Leverage Plot. These outliers
#can and do lead to inaccurate estimates of regression coefficients.

#3)Based on the Scale Location plot, 
#There MAY be heteroskedasticity (violation of the constant variance assumption). Notice
#the upward trend of the red line. Again, I am skeptical of this though, and it may be
#the outliers causing problems.

#e) 


cooksd = cooks.distance(reg)
cooksd

absResid = abs(reg$residuals)


absResid = as.data.frame(absResid)
absResidOrdered = absResid %>% 
  arrange(desc(absResid))
View(absResidOrdered)
dim(absResidOrdered)

#Grape-Nuts, Great Grains Pecan, and Bran Chex appear to be the largest outliers 
#(based on the absolute value of their residuals and the Residuals vs Leverage Plot)


#f)

#having trouble accessing a names attribute for the UScereal data so I manually
#selected which rows to delete based on the absolute value of the residuals, which I 
#calculated above


updatedData = UScereal %>%
  arrange(desc(carbo)) 

View(updatedData)

#remove Grape-Nuts, Great Grains Pecan, and Bran Chex
outliersRemovedData = updatedData[-c(1,2,16),]


#run a new regression
newReg = lm(calories ~ protein + fat + fibre + carbo + sugars, data = outliersRemovedData)
summary(newReg)

par(mfrow = c(1,1))
plot(newReg)

#1 the residual standard error decreased from 8.932 to 5.736. Our model is now more precise

#2 Based on the Residuals vs Fitted plot, the new regression model appears to be more
#linear than it had appeared in the previous plot with the outliers.

#3 The F Statistic increased from about 613 to 689.5, indicating that the overall 
#significance of the regressors in the model is greater than it was previously with the 
#outliers.

#4 R^2 had a minor increase.

#5 Based on the Scale Location plot, the constant variance assumption appears to be more
#appropriate having removed three outliers.

#The distribution of the errors appears to be distributed more normally than it had been
#previously thanks to the removal of the outliers (based on QQplot).




  
  






  
  