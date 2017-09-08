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
gapminder$continent <- as.character(gapminder$continent)
gapminder$country <- as.character(gapminder$country)
gapminder$new_continent <- ifelse(gapminder$continent=="Americas", 
                                  ifelse(gapminder$country %in% northamerica,
                                      "North America",
                                      "South America"), 
                                  gapminder$continent)


# Make a new object, gapminder_max that has only one observation per country, and has the maximum value that each country has had overtime for population, life expectancy, and GDP per capita. Hint: this step is a little more straightforward in dplyr than in base R; in base R, use aggregate and note that the first argument (the data) needs to only contain the columns you want to compute a summary measure on. Hint 2: for extra dplyr finesse, take a look at the dplyr function summarize_at.
library(plyr)
gapminder_max1 <- gapminder %>% group_by(country) %>% slice(which.max(pop))
gapminder_max2 <- gapminder %>% group_by(country) %>% slice(which.max(lifeExp))
gapminder_max3 <- gapminder %>% group_by(country) %>% slice(which.max(gdpPercap))

gapminder_max <- rbind.fill(gapminder_max1,gapminder_max2,gapminder_max3)
gapminder_max <- gapminder_max[!duplicated(gapminder_max), ]

##### Exercise: Grouping and Summarizing
# Calculate the average life expectancy per country. Which had the longest life expectancy and which had the shortest life expectancy?
gapminder_meanlife <- gapminder %>% dplyr::group_by(country) %>% dplyr::summarise(meanLife = mean(lifeExp))
gapminder_meanlife %>% slice(which.max(meanLife))
gapminder_meanlife %>% slice(which.min(meanLife))

##### Exercise: Random Subsets
# Calculate the average life expectancy in 2002 of 2 randomly selected countries for each continent. Then arrange the continent names in reverse order. Do this with dplyr; it’s very messy to do with base R and would likely use things we haven’t learned yet (no base R answer provided).
gapminder_randomcountry <- gapminder %>% group_by(continent) %>% sample_n(2)
gapminder_randomcountry %>% dplyr::group_by(continent) %>% dplyr::summarise(meanLife = mean(lifeExp)) %>% arrange(meanLife)
# Hint: Use the dplyr functions arrange() and sample_n(), they have similar syntax to other dplyr functions.





