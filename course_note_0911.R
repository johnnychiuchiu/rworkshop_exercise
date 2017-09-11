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
     
    
                       
