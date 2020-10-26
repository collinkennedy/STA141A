# sta 141a fall 2020
# instructor: vaidotas characiejus



### ggplot2
# hadley wickham's tidyverse
# https://www.tidyverse.org (r packages for data science)
# install.packages("tidyverse")
# https://ggplot2.tidyverse.org (ggplot2 is a system for declaratively creating graphics, based on the grammar of graphics (https://www.springer.com/it/book/9780387245447) which is a coherent system for describing and building graphs)
# https://r4ds.had.co.nz/data-visualisation.html



library(ggplot2)
#?mpg
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
# ggplot() is used to construct the initial plot object, and is almost always followed by + to add component to the plot
# ggplot() creates a coordinate system that you can add layers to
# geom_point() adds a layer of points to your plot, which creates a scatterplot
# each geom function in ggplot2 takes a mapping argument
# the mapping argument is always paired with aes(), and the x and y arguments of aes() specify which variables to map to the x and y axes
# aesthetic mappings describe how variables in the data are mapped to visual properties (aesthetics) of geoms
# ggplot2 comes with many geom functions that each add a different type of layer to a plot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
 geom_smooth() +
 geom_point()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
 geom_smooth(se = FALSE, method = "lm") +
 geom_point()



### aesthetic mappings
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = class))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
# Warning message:
# Using size for a discrete variable is not advised.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = cty))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
# only uses six shapes at a time
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), colour="red")
# here, the color doesn’t convey information about a variable, but only changes the appearance of the plot
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), shape=5)
 


#### geometric objects
# a geom is the geometrical object that a plot uses to represent data
# compare the following two graphs
ggplot(data = mpg) +
	geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +
	geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +
	geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
# https://www.rstudio.com/resources/cheatsheets/



# global and local mappings
ggplot(data = mpg) +
	geom_point(mapping = aes(x = displ, y = hwy)) +
	geom_smooth(mapping = aes(x = displ, y = hwy))
# global mappings
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
	geom_point() +
	geom_smooth()
# local mappings
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
	geom_smooth(se = FALSE)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
	geom_smooth(mapping = aes(linetype = drv)) +
	geom_point(mapping = aes(color = drv))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
	geom_point(mapping = aes(color = drv)) +
	geom_smooth(mapping = aes(linetype = drv, color = drv))



### facets
# One way to add additional variables is with aesthetics. Another way, particularly useful for categorical variables, is to split your plot into facets, subplots that each display one subset of the data.
# facet_grid() forms a matrix of panels defined by row and column faceting variables. It is most useful when you have two discrete variables, and all combinations of the variables exist in the data. If you have only one variable with many levels, try facet_wrap().
ggplot(data = mpg) +
	geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
	facet_wrap(~ class, nrow = 2)
ggplot(data = mpg) +
	geom_point(mapping = aes(x = displ, y = hwy)) +
	facet_grid(drv ~ cyl)
sum(mpg[mpg$drv==4,]$cyl==5)
ggplot(data = mpg) +
	geom_point(mapping = aes(x = displ, y = hwy)) +
	facet_grid(. ~ class)
ggplot(data = mpg) +
	geom_point(mapping = aes(x = displ, y = hwy)) +
	facet_grid(. ~ cyl)
# which is the same as
ggplot(data = mpg) +
	geom_point(mapping = aes(x = displ, y = hwy)) +
	facet_wrap(~ cyl, nrow = 1)



#  the group aesthetic by itself does not add a legend or distinguishing features to the geoms
ggplot(data = mpg) +
	geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +
	geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
ggplot(data = mpg) +
	geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))
ggplot(data = mpg) +
	geom_smooth(mapping = aes(x = displ, y = hwy, color = drv),show.legend=FALSE)



# subset of the mpg dataset
# library(tidyverse)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
	geom_point(mapping = aes(color = class)) +
	geom_smooth(data = dplyr::filter(mpg, class == "subcompact"), se = FALSE)
# local data argument in geom_smooth() overrides the global data argument
# ?geom_smooth
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
	geom_smooth(data = dplyr::filter(mpg, class != "2seater"), se = FALSE, span = .75) +
	geom_point(mapping = aes(color = class))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(se = FALSE, span = .4) +
  geom_point(mapping = aes(color = class))
unique(dplyr::filter(mpg, class != "2seater")$class)



### statiscal transformations
?diamonds
ggplot(data = diamonds)	+
	geom_bar(mapping = aes(x = cut))
# On the x-axis, the chart displays cut, a variable from diamonds. On the y-axis, it displays count, but count is not a variable in diamonds! Where does count come from?
# Many graphs, like scatterplots, plot the raw values of your dataset. Other graphs, like bar charts, calculate new values to plot.
# The algorithm used to calculate new values for a graph is called a stat, short for statistical transformation.
# geom_bar()
?geom_bar
# stat = "count"
# stats and geoms can be generally used interchangeably
ggplot(data = diamonds) + 
	stat_count(mapping = aes(x = cut))
margin.table(Titanic,1)
df<-data.frame(margin.table(Titanic,1))
ggplot(data = df) +
  geom_bar(mapping = aes(x = Class, y = Freq))	
ggplot(data = df) +
	geom_bar(mapping = aes(x = Class, y = Freq),stat="identity")	



# colouring bar plots
ggplot(data = diamonds) + 
	geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
	geom_bar(mapping = aes(x = cut, fill = cut), show.legend=FALSE)
ggplot(data = diamonds) +
	geom_bar(mapping = aes(x = cut, fill = clarity))



### position adjustements
# dealing with overplotting in a scatter plot
# jitter
ggplot(data = mpg) + 
	geom_point(mapping = aes(x = displ, y = hwy))
# the plot displays only 126 points, even though there are 234 observations in the dataset
ggplot(data = mpg) + 
	geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
ggplot(data = mpg) + 
	geom_jitter(mapping = aes(x = displ, y = hwy),width = 0.2,color=alpha("black",.5))
# geom_bin2d
# histogram
# an alternative to geom_point() in the presence of overplotting
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
	geom_point()
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
	geom_bin2d()
# install.packages("hexbin")
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
	geom_hex(bins = 70)
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
	geom_hex(bins = 50)
ggplot(data = diamonds, mapping = aes(x = carat, y = price, colour = cut)) +
	geom_hex(bins = 50)
ggplot(data = diamonds,mapping = aes(x = carat, y = price)) +
	geom_hex() +
	scale_fill_gradient(low="grey",high="red")


	
### coordinate systems
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
	geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
	geom_boxplot() +
	coord_flip()
#
df<-tibble(Model=rownames(mtcars),mpg=mtcars$mpg)
ggplot(data = df) +
	geom_bar(mapping = aes(x = reorder(Model,mpg), y = mpg),stat="identity") +
	labs(title = "Fuel economy",x = "Model",y = "Miles per Galon") +
	coord_flip()
	
 

### graphics for communication
# from exploration to communication	
# install.packages("tidyverse")
library(tidyverse)
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency generally decreases with engine size",
  	subtitle = "Two seaters (sports cars) are an exception because of their light weight",
  	x = "Engine displacement (in litres)",
  	y = "Highway miles per gallon",
  	caption = "Data from fueleconomy.gov",
  	colour = "Car type")

# annotations
best_in_class <- mpg %>%
	group_by(class) %>%
	filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
	geom_point(aes(colour = class)) +
	geom_text(aes(label = model), data = best_in_class)
  
ggplot(mpg, aes(displ, hwy)) +
	geom_point(aes(colour = class)) +
	geom_label(aes(label = model), data = best_in_class, nudge_y = 2, alpha = .5)
#install.packages("ggrepel")
ggplot(mpg, aes(displ, hwy)) +
	geom_point(aes(colour = class)) +
	geom_point(size = 1, shape = 1, data = best_in_class) +
	ggrepel::geom_label_repel(aes(label = model), data = best_in_class)
	

  
# themes
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw()
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_classic()
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_dark()
# legend
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  theme(legend.position = "left")
# left, top, bottom, right (default)



# saving plots
ggplot(mpg, aes(displ, hwy)) +
	geom_point()
ggsave("scatterplot.pdf", width = 6, height = 6, scale = 1)
ggsave("scatterplot.png", width = 6, height = 6, scale = 1)