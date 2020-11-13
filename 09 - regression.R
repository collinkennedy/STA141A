# sta 141a fall 2020
# instructor: vaidotas characiejus


library(ggplot2)

tv_sp<-qplot(x = TV, y = sales, data = Advertising)
radio_sp<-qplot(x = radio, y = sales, data = Advertising)
newspaper_sp<-qplot(x = newspaper, y = sales, data = Advertising)

# install.packages("gridExtra")
gridExtra::grid.arrange(tv_sp, radio_sp, newspaper_sp,ncol=1)

# multiple linear regression
lm(sales ~ TV + radio + newspaper, data = Advertising)
lreg<-lm(sales ~ TV + radio + newspaper, data = Advertising)
summary(lreg)



# design matrix
attach(Advertising)
X<-cbind(1,TV,radio,newspaper) #design matrix

# ols estimators
beta_ols<-solve(t(X)%*%X)%*%t(X)%*%sales #matrix of coefficients
#matrix of our parameters for intercept, tv, radio, and newspaper


# estimate of the error variance
names(lreg)
sqrt(sum((sales-X%*%beta_ols)^2)/196) #sales - X(matrixmult)beta_ols gives us our fitted values
s<-sqrt(sum(lreg$res^2)/lreg$df.residual)
summary(lreg)

# estimates of the standard errors
se<-sqrt(diag(s^2*solve(t(X)%*%X)))

# t values
t_values<-lreg$coeff/se

t# P(>|t|)
2*pt(abs(t_values),196,lower.tail=FALSE)#pt gives the CDF of t dist.
confint(lreg)

# f-statistic
tss<-sum((sales-mean(sales))^2)
rss<-sum(lreg$res^2)
F<-((tss-rss)/3)/(rss/lreg$df.residual)
pf(F,3,196,lower.tail=FALSE)

# r-squared
tss<-sum((sales-mean(sales))^2)
rss<-sum(lreg$res^2)
rsq<-1-rss/tss

# adjusted r-squared
1-(1-rsq)*((200-1)/lreg$df.residual)
