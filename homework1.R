animals = c("cat","dog","cow","squirrel")
colors = c("white","black","brown","red")
attr = c("big","small","angry","cute", "finicky")

listOfVecs = list(attr, colors, animals)
randomPhraseVector = c()
for(vector in listOfVecs){
  appendWord = sample(vector,1)
  randomPhraseVector = append(randomPhraseVector,appendWord)
  
}
paste(randomPhraseVector)


A = matrix(c(1,2,3,4,5,
             2,1,2,3,4,
             3,2,1,2,3,
             4,3,2,1,2,
             5,4,3,2,1),5,5,byrow = TRUE)

A

y = c(7,-1,-3,5,17)
solve(A,y)


load("/Users/collinkennedy/Google Drive/Programming Folder (4Transfer to Collin's Files V2)/R/STA141A/countypop.RData")

View(countypop)

counter <- 0
countypop$abbr
vecOfAbbrevs <- c()
for (abbrev in countypop$abbr){
  if (abbrev in vecOfAbbrevs){
    next
}else{
counter = counter + 1
append(vecOfAbbrevs,abbrev)
}

  
}
counter

x <- c(2,5,3,9,8,11,6)
count <- 0
for (val in x) {
  if(val %% 2 == 0)  count = count+1
}
print(count)

vecOfAbbrevs <- unique(countypop$abbr)
length(vecOfAbbrevs)

length(countypop$county)
    

vecOfUniqueCounties = unique(countypop$county)
length(vecOfUniqueCounties)
    
table(countypop$county)
library(dplyr)

countypop %>% arrange(county)

tally(countypop$county)


summarise(countypop$county)

#d
countyFreq <- as.data.frame(table(countypop$county))
countyFreq = countyFreq %>% arrange(desc(Freq))
countyFreq


#e
countyAbbr <- as.data.frame(table(countypop$abbr))
countyAbbr  = countyAbbr %>% arrange(desc(Freq))
countyAbbr 

#g
subDF = countypop %>% arrange(desc(pop_2015))
state = subDF$abbr[1]
state

pop = subDF$pop_2015[1]
pop

#**h)** *What is the largest county in terms of population of each of the states (2
#get population of each of the states
subDF = countypop %>% arrange(desc(pop_2015)) #arrange a subset by population in desc order
county = subDF$county[1]
View(subDF)
#the largest county in the US by population:
county

#the largest
pop = subDF$pop_2015[1]
pop

hDF = countypop %>%
  group_by(abbr) %>% 
  summarise(largestPop = which.max(pop_2015))
View(hDF)

hDF2 = countypop %>%
  group_by(abbr) %>%
  filter(abbr != "DC") %>% 
  slice(which.max(pop_2015))#slice selects rows by position->in this case
                            #it chooses the greatest population value of each county
                            #since we are grouping by State (abbr)

print(hDF2[1:50,])

#i
countyPopByPop = countypop %>% arrange(desc(pop_2015))
mean(countyPopByPop$pop_2015[1:100])

#jhow many people live in each of the states
totalPopDF = group_by(countypop, abbr) %>% 
  summarize(totalPopulation = sum(pop_2015)) %>%
  filter(abbr != "DC") %>% 
  arrange(desc(totalPopulation))
print(totalPopDF, n= 50)


#k
caliDF = countypop %>% filter(abbr == "CA")

mean(caliDF$pop_2015)


meanPopDF = group_by(countypop, abbr) %>% 
summarize(meanPopulation = mean(pop_2015)) %>%
  filter(abbr != "DC") %>% 
  arrange(desc(meanPopulation))
print(meanPopDF, n= 50)



