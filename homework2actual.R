
#1
library(dplyr)
library(plyr)
install.packages("plyr")
??rdply

set.seed(2)
randomBinomSample = rbinom(50,10,.4) #generate a random sample of 50 observations from
                                      #binomial distribution where #trials = 10, p =.4
randomBinomSample


#2

#something is wrong with the denominator I think
estimateKHat = function(randomBinSamp){
  sampleMean = mean(randomBinSamp) #calculate Xbar
  oneOverN = (length(randomBinSamp)-1)/(length(randomBinSamp)) #(n-1)/n to get populationvar
  popVar = oneOverN*var(randomBinSamp) #actually calculate population variance
  denominator = sampleMean - popVar
  kHat = ((sampleMean)^2)/(denominator)
  return(kHat)
}

estimateKHat2 = function(randomBinSamp){
  sampleMean = mean(randomBinSamp) #calculate Xbar
  
  kHat = ((sampleMean)^2)/(sampleMean-1/(length(randomBinSamp))*(sum((randomBinSamp-sampleMean)^2)))
  return(kHat)
}
estimateKHat2(randomBinomSample)
  
estimatePHat = function(randomBinSamp){
  sampleMean = mean(randomBinSamp) #calculate the mean of the binomial sample vector
  returnedKHat = estimateKHat(randomBinSamp)
  pHat = sampleMean/returnedKHat
  return(pHat)
}
estimatePHat(randomBinomSample)



#3
#generate 1000 random samples of size 50
randomSamples = list(replicate(1000,rbinom(50,10,.4)))
randomSamples

randomSamplesDF = data.frame(randomSamps = randomSamples)


randomSampleDF = t(randomSamplesDF)
estimatedKHats = apply(randomSampleDF,1,estimateKHat)
estimatedKHats
mean(estimatedKHats)

#create a vector of estimated PHats
estimatedPHats = apply(randomSampleDF,1,estimatePHat)



#5
bias = function(vec,ev){
  expValEstimatorHat = mean(vec)
  return(expValEstimatorHat-ev)
}
mse = function(vec,ev){
  expValEstimatorHat = mean(vec)
  sse = sum((vec - ev)^2)
  meanSquaredError = sse/length(vec)
  return(meanSquaredError)
  
}
#when n=50
set.seed(2)
randomSamples = list(replicate(1000,rbinom(50,10,.4)))

randomSamplesDF = data.frame(randomSamps = randomSamples)

randomSampleDF = t(randomSamplesDF)
View(randomSampleDF)

estimatedKHats50 = apply(randomSampleDF,1,estimateKHat)
which.max(estimatedKHats50)
estimatedKHats50[996]

estimatedPHats = apply(randomSampleDF,1,estimatePHat)

#calculate bias for kHat when n=50
bias(estimatedKHats,10)

#calculate mse for kHat when n = 50
mse(estimatedKHats,10)

#calculate bias for pHat when n =50
bias(estimatedPHats,.4)

#calculate mse for pHat when n = 50
mse(estimatedPHats,.4)

#when n=100
set.seed(2)
randomSamples = list(replicate(1000,rbinom(100,10,.4)))

randomSamplesDF = data.frame(randomSamps = randomSamples)

randomSampleDF = t(randomSamplesDF)

estimatedKHats100 = apply(randomSampleDF,1,estimateKHat)

estimatedPHats = apply(randomSampleDF,1,estimatePHat)


#calculate bias for kHat when n=100
bias(estimatedKHats,10)

#calculate mse for kHat when n = 100
mse(estimatedKHats,10)

#calculate bias for pHat when n =100
bias(estimatedPHats,.4)

#calculate mse for pHat when n = 100
mse(estimatedPHats,.4)




#when n = 250

set.seed(2)
randomSamples = list(replicate(1000,rbinom(250,10,.4)))

randomSamplesDF = data.frame(randomSamps = randomSamples)

randomSampleDF = t(randomSamplesDF)

estimatedKHats250 = apply(randomSampleDF,1,estimateKHat)

estimatedPHats = apply(randomSampleDF,1,estimatePHat)


#calculate bias for kHat when n=250
bias(estimatedKHats,10)

#calculate mse for kHat when n = 250
mse(estimatedKHats,10)

#calculate bias for pHat when n =250
bias(estimatedPHats,.4)

#calculate mse for pHat when n = 250
mse(estimatedPHats,.4)



infoMatrix = matrix(1:12, nrow=4,ncol = 3 )
rownames(infoMatrix)=c("kHat MSE","kHat Bias","pHat MSE", "pHat Bias")
infoMatrix
colnames(infoMatrix) = c("n=50","n=100","n=250")
infoMatrix[1:4,1]= c(711.1065,1.7945,.0134,.0058)

infoMatrix[1:4,2] = c(8.1532,.3849,.0065,.0049)

infoMatrix[1:4,3] = c(1.6885,.0576,.0024,.0039)

infoMatrix




#6
library(ggplot2)
vecKHats = c(estimatedKHats50,estimatedKHats100,estimatedKHats250)
ens = c(rep(50,1000),rep(100,1000),rep(250,1000))
vecKHats
ggplot

kHatDF = data.frame(kHats = vecKHats,n = ens)
View(kHatDF)
mean(kHatDF[1:1000,1])
mean(kHatDF[1001:2000,1])
kHatDF$n = as.factor(kHatDF$n)
View(kHatDF)
which.max(kHatDF$kHats)


kHatBoxPlot <- ggplot(kHatDF) + 
  geom_boxplot(mapping = aes(x=n, y=kHats,color = n))+
  geom_hline(yintercept = 10)+
  labs(title="Plot of kHats at each Sample Size ",x="sample size (n)", y = "K Estimates")

kHatBoxPlot

kHatBoxPlotRestricted <- ggplot(kHatDF) + 
  geom_boxplot(mapping = aes(x=n, y=kHats,color = n))+
  geom_hline(yintercept = 10)+
  labs(title="Plot of kHats at each Sample Size ",x="sample size (n)", y = "K Estimates")+
  coord_cartesian(ylim = c(0, 50))

kHatBoxPlotRestricted


#b

#when n=50
set.seed(2)
randomSamples = list(replicate(1000,rbinom(50,10,.4)))

randomSamplesDF = data.frame(randomSamps = randomSamples)

randomSampleDF = t(randomSamplesDF)
View(randomSampleDF)
estimatePHat(randomSampleDF[706,])
mean(randomSampleDF[706,])

###RIGHT HERE
estimateKHat(randomSampleDF[706,])

estimatedKHats50 = apply(randomSampleDF,1,estimateKHat)
which.max(estimatedKHats50)

estimatedPHats50 = apply(randomSampleDF,1,estimatePHat)



#when n=100
set.seed(2)
randomSamples = list(replicate(1000,rbinom(100,10,.4)))

randomSamplesDF = data.frame(randomSamps = randomSamples)

randomSampleDF = t(randomSamplesDF)

estimatedKHats100 = apply(randomSampleDF,1,estimateKHat)

estimatedPHats100 = apply(randomSampleDF,1,estimatePHat)




#when n = 250

set.seed(2)
randomSamples = list(replicate(1000,rbinom(250,10,.4)))

randomSamplesDF = data.frame(randomSamps = randomSamples)

randomSampleDF = t(randomSamplesDF)

estimatedKHats250 = apply(randomSampleDF,1,estimateKHat)

estimatedPHats250 = apply(randomSampleDF,1,estimatePHat)

vecPHats = c(estimatedPHats50,estimatedPHats100,estimatedPHats250)
pHatDF = data.frame(pHats = vecPHats,n = ens)
pHatDF$n = as.factor(pHatDF$n)

which.min(estimatedPHats50)
estimatedPHats50[706]

library(dplyr)
pHatDF = pHatDF %>% 
  filter(pHats >= 0)



pHatBoxPlot <- ggplot(pHatDF) + 
  geom_boxplot(mapping = aes(x=n, y=pHats,color = n))+
  geom_hline(yintercept = .4)+
  labs(title="Plot of pHats at each Sample Size ",x="sample size (n)", y = "P Estimates")


pHatBoxPlot

