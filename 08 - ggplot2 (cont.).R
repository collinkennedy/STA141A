# sta 141a fall 2020
# instructor: vaidotas characiejus



### simplified version
# qplot is a shortcut designed to be familiar if you are used to base plot()
# instead of using
ggplot(data = mpg) + 
	geom_point(mapping = aes(x = displ, y = hwy))
# we can use
qplot(displ, hwy, data = mpg)
# defaults to geom_point() if x and y are specified, and geom_histogram() if only x is specified
qplot(mpg, data = mtcars)

qplot(wt, mpg, data = mtcars, color = I("red"), size = I(1))
qplot(wt, mpg, data = mtcars, colour = ordered(cyl), size = I(2))
qplot(wt, mpg, data = mtcars, colour = ordered(cyl), size = I(2), ylab="Miles per Galon", xlab="Weight (1000 lbs)", main = "Fuel efficiency") + labs(colour = "Cylinders")
qplot(wt, mpg, data = mtcars, colour = ordered(cyl), geom = c("point","smooth"))


# saving plot as an object
mainplot<-qplot(wt, mpg, data = mtcars, colour = factor(cyl))
mainplot + geom_smooth()
mainplot + geom_smooth(span = 2, se = FALSE)
mainplot + geom_smooth(mapping = aes(colour = NULL))
save(mainplot,file="mainplot.RData")


mainplot + geom_smooth(method = "lm")
mainplot + geom_smooth(method = "lm", size = 2)