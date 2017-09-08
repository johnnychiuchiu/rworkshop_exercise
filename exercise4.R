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


