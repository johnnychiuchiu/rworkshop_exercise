library(tidyverse) 
gapminder<-read_csv("https://goo.gl/dWrc9m")
library(gapminder)
data(gapminder)

##### Exercise: Explore the Structure of the Dataset
# What are the names of the columns?
colnames(gapminder)

# Is there any missing data?
colSums(is.na(gapminder))

# How many different countries are there?
table(gapminder$country)
length(unique(gapminder$country))

# What is the continent name for the US?
unique(gapminder[gapminder$country=='United States',]$continent)

# Does the number of countries in the data change over time? No
library(dplyr)
gapminder %>% group_by(year) %>% summarise(n_distinct(country))



##### Exercise: Selecting and Filtering
# Show observations where life expectancy is greater than 80.
gapminder %>% filter(lifeExp > 80)

# Show only population and GDP per capita for Kenya for years before 1970.
gapminder %>% filter(year< 1970 & country=='Kenya') %>% select(pop, gdpPercap)

# Show the observation that has the maximum life expectancy
gapminder %>% filter(lifeExp == max(gapminder$lifeExp))


##### Exercise: Transforming Data
# Add a new column to gapminder that is the population in millions.
gapminder<- gapminder %>% mutate(popInMill=round(pop/1000000,2))

# Instead of having North and South American countries together as a continent, reassign the North American countries in the vector below to have contient “North America” and the rest of the countries currently associated with “America” to be “South America”. Hint: continent is a factor, so you’ll need to either first convert it to character data or explicitly manipulate the levels. Hint 2: there isn’t a natural workflow for this situation in dplyr. It can be done, but you probably need the ifelse function, and you’ll end up strining multiple mutate commands together.
# not all of them, but these are the ones in gapminder
northamerica <- c("Canada", "Costa Rica", "Cuba", "Dominican Republic", 
                  "El Salvador", "Guatemala", "Haiti", "Honduras",
                  "Jamaica", "Mexico", "Nicaragua", "Panama",
                  "Trinidad and Tobago", "United States")
# gapminder$continent <- as.character(gapminder$continent)
# gapminder$country <- as.character(gapminder$country)
# gapminder$new_continent <- ifelse(gapminder$continent=="Americas", 
#                                   ifelse(gapminder$country %in% northamerica,
#                                       "North America",
#                                       "South America"), 
#                                   gapminder$continent) #change the script into mutate with ifelse
#### it's a better solution
gapminder <- gapminder %>%
  mutate(continent = as.character(continent), country = as.character(country)) %>%
  mutate(continent = ifelse(country %in% northamerica, 
                            "North America", 
                            ifelse(continent=="Americas",
                                   "South America",
                                   continent))) %>%
  mutate(continent = factor(continent))


# Make a new object, gapminder_max that has only one observation per country, and has the maximum value that each country has had overtime for population, life expectancy, and GDP per capita. Hint: this step is a little more straightforward in dplyr than in base R; in base R, use aggregate and note that the first argument (the data) needs to only contain the columns you want to compute a summary measure on. Hint 2: for extra dplyr finesse, take a look at the dplyr function summarize_at.
library(plyr)
# gapminder_max1 <- gapminder %>% group_by(country) %>% slice(which.max(pop))
# gapminder_max2 <- gapminder %>% group_by(country) %>% slice(which.max(lifeExp))
# gapminder_max3 <- gapminder %>% group_by(country) %>% slice(which.max(gdpPercap))
# 
# gapminder_max <- rbind.fill(gapminder_max1,gapminder_max2,gapminder_max3)
# gapminder_max <- gapminder_max[!duplicated(gapminder_max), ]
#### it's a better solution
gapminder_max <- gapminder %>%
  group_by(country, continent) %>% # include continent so it's in the final data
  summarize_at(vars(pop, lifeExp, gdpPercap), max) # applies same function to multiple columns

# with the standard summarize instead of summerize_at:
gapminder_max <- gapminder %>%
  group_by(country, continent) %>%
  summarize(pop_max = max(pop),
            lifeExp_max = max(lifeExp),
            gdpPercap_max = max(gdpPercap))


##### Exercise: Grouping and Summarizing
# Calculate the average life expectancy per country. Which had the longest life expectancy and which had the shortest life expectancy?
# gapminder_meanlife <- gapminder %>% dplyr::group_by(country) %>% dplyr::summarise(meanLife = mean(lifeExp))
# gapminder_meanlife %>% slice(which.max(meanLife))
# gapminder_meanlife %>% slice(which.min(meanLife))
#### it's a better solution
# dplyr, first with a little trick using arrange and row_number()
gapminder %>%
  group_by(country) %>%
  summarize(mean_lifeExp=mean(lifeExp)) %>%
  arrange(mean_lifeExp) %>%
  filter(row_number() %in% c(1, n())) 

# dplyr, doing it in a more strightforward way
gapminder %>%
  group_by(country) %>%
  summarize(mean_lifeExp=mean(lifeExp)) %>%
  filter(mean_lifeExp == min(mean_lifeExp) | 
           mean_lifeExp == max(mean_lifeExp))


##### Exercise: Random Subsets
# Calculate the average life expectancy in 2002 of 2 randomly selected countries for each continent. Then arrange the continent names in reverse order. Do this with dplyr; it’s very messy to do with base R and would likely use things we haven’t learned yet (no base R answer provided).
gapminder %>% group_by(continent) %>% sample_n(2) %>% dplyr::summarise(meanLife = mean(lifeExp)) %>% arrange(meanLife)
# Hint: Use the dplyr functions arrange() and sample_n(), they have similar syntax to other dplyr functions.


##### Challenge Exercise: Complex Group Summary
# Find all countries where life expectancy increased by at least 60% from 1952 to 2007.
# Hint for dplyr: Remember that with dplyr, summarize computes one value for each group, while mutate computes one value for each row. Also, once data is grouped, you can index variables relative to their position in their group.
# Hint for base R: you might find the merge function useful.
gapminder1952 <- gapminder %>% filter(year=='1952') %>% select(country, lifeExp)
colnames(gapminder1952)[which(names(gapminder1952) == "lifeExp")] <- "lifeExp_1952"

gapminder_calculate <-merge(gapminder,gapminder1952,by='country',all.x= TRUE)
gapminder_calculate$increPercent <- round(gapminder_calculate$lifeExp/gapminder_calculate$lifeExp_1952,3)
gapminder_calculate_final <-gapminder_calculate %>% filter(increPercent>1.6)
unique(gapminder_calculate_final$country)

#### it's a better solution
# dplyr
gapminder %>%
  group_by(country) %>%
  dplyr::arrange(year) %>%
  dplyr::summarize(pctIncrease = (lifeExp[n()] - lifeExp[1])/lifeExp[1]) %>%
  dplyr::filter(pctIncrease >= 0.6)



##### Challenge Exercise: Drops in Population
# Find all countries that experienced a drop in population at any point in the timespan of the data.
# Hint: look at the diff base R function or dplyr functions lead and lag (search help for “lead-lag”). Note that diff will result in one fewer values in the result than in the original, so you’ll need to address that.
gapminder_drop <- gapminder %>% mutate(popLag=lag(pop))
gapminder_drop$diff <- gapminder_drop$pop - gapminder_drop$popLag
gapminder_drop <- gapminder_drop %>% filter(year!=1952) %>% filter(diff<0)
length(unique(gapminder_drop$country))

#### it's a better solution
# dplyr
gapminder %>%
  dplyr::group_by(country) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(popdiff = pop-lag(pop)) %>%
  dplyr::filter(!is.na(popdiff) & popdiff < 0) #plyr and dplyr are often conflict and product unexpected result.