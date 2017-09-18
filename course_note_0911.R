library(tidyverse)
library(gapminder)
library(babynames)

gapminder<-read_csv("https://goo.gl/dWrc9m")
schooldata <- read_csv("~/team/bootcamp/R/schooldata.csv")

### calculate the top baby names, and show the most evenly used names for Male and Female.
View(babynames)
topnames <- babynames %>%
  filter(year >= 1950) %>%
  dplyr::group_by(name) %>%
  dplyr::summarize(total=sum(n)) %>%
  arrange(desc(total)) %>%
  slice(1:1000) %>%
  inner_join(babynames, by='name') %>%
  filter(year >= 1950) %>%
  mutate(sex=recode(sex, "F"="Female", "M"="Male")) %>%
  dplyr::group_by(name, sex, total) %>%
  dplyr::summarise(sextotal=sum(n)) %>% 
  spread(key=sex, value=sextotal) %>% 
  mutate(ratio=(Male-Female)/total)

ggplot(topnames, aes(x=Male, y=Female)) + geom_point()

ggplot(topnames, aes(x=Male, y=Female, color=ratio)) +
  geom_point()+
  lims(x=c(0,250000),y=c(0,250000)) +
  geom_abline() +
  scale_color_gradient(low='pink',high='blue',
                       name='Sex Dominance',
                       breaks=c(.9,0,-.9),
                       labels=c("Male","Neutral","Female")) +
  geom_text(aes(label=ifelse(abs(ratio)<.2,
                             name,
                             '')),
            hjust=-.25, vjust=.5, color='gray10', 
            fontface='bold', size=3)

### plot bar charts for different names
topnames2 <- gather(topnames, key="sex", value="sextotal", Male, Female)

topnames2 %>% 
  filter(abs(ratio)< .3 & !is.na(ratio)) %>%
  ggplot(aes(x=name,y=sextotal,fill=sex)) + 
  geom_bar(stat='identity', position="fill") +
  geom_hline(yintercept=.5) +
  labs(title="Popular Unisex Names", x="", y="Count") 
  
topnames2 %>% 
  filter(abs(ratio)< .3 & !is.na(ratio)) %>%
  ggplot(aes(x=name,y=sextotal,fill=sex)) + 
  geom_bar(stat='identity', position="dodge") +
  geom_hline(yintercept=.5) +
  labs(title="Popular Unisex Names", x="", y="Count") 

topnames2 %>% 
  filter(abs(ratio)< .3 & !is.na(ratio)) %>%
  ggplot(aes(x=name,y=sextotal,fill=sex)) + 
  geom_bar(stat='identity', position="stack") +
  geom_hline(yintercept=.5) +
  labs(title="Popular Unisex Names", x="", y="Count") 

library(ggthemes)
hc_plot <- topnames2 %>% 
  filter(abs(ratio)< .3 & !is.na(ratio)) %>%
  ggplot(aes(x=name,y=sextotal,fill=sex)) + 
  geom_bar(stat='identity', position="stack") +
  geom_hline(yintercept=.5) +
  labs(title="Popular Unisex Names", x="", y="Count") +
  theme_hc() 
  
library(plotly)
ggplotly(hc_plot)

  
# https://github.com/jrnold/ggthemes
     
###
library(tidyverse)
library(stringr)

x <- c("\"", 
       "\\", #escape with \,
       "\t", #change line 
       "\u00b5") #unicode
writeLines(x)

str_length(c("R","R for data science"))

fruit <- c("apple","banana",'pear')
str_sub(fruit, 1, 3)
str_view(fruit, "an")
str_view(fruit,".a.") # . : match one and exactly one character
str_view(c("127.0.0.1"),"\\.") # why do we need two \\?
str_view(fruit,"^a") # ^ : match the beginning of the string to be "a"
str_view(fruit,"a$") # $ : match the ending of the string to be "a"

apples <- c('apple pie', 'apple', 'apple cake')
str_view(apples, "^apple$") # apple to be the beginning, and apple to be the end. It treat apple as a whole.

str_view("127.0.0.1", "\\d\\.\\d")
str_view_all("127.0.0.1", "\\d\\.\\d")

str_view(c('gray','grey'), "gr[ae]y") # match both a & e for the third character
str_view(c('gray','grey'), "gr(a|e)y") # match both a & e for the third character

str_view(fruit, "^[^a]") # character class all except a

x <- "MDCCCLXXXVIII" #1888
str_view(x, "CC+") # match the item that has at least two C's in x , greedy.
str_view(x, "CC?") # not greedy, ? represent 0 or 1, (not greedy, match the minimum)
str_view(x, "CC*") # greedy, (greedy, match the maxmimum)

str_view(x, "C[LX]+") # C + LorX greedy
str_view(c(fruit, "nnaa"),'(na)+') #n have to be followed by a with indefinite times

str_view(x,"C{2}") # exactly 2
str_view(x, "C{2,}") # 2 or more
str_view(x, "C{0,2}") # up to 2

str_view(x,"CC*?") # make * not greedy, ? makes * not greedy
str_view(x,"CC+?") # make * not greedy, ? makes * not greedy

fruit <- c("banana", "coconut", "cucumber", "jujube", "papaya", "salal berry")
str_view(fruit, "(..)\\1")
str_view(fruit, "(..){2}")

str_view(c('a a','abc'),"\\s")
