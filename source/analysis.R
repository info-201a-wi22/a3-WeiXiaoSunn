# Dataset
library(tidyverse)
#Data from github repository
raw_data <- read.csv("incarceration_trends_preded.csv") 
#load the new data for only time period 2010-2018
new_data <- raw_data %>% filter(year>=2010) %>% filter(year<=2018)

# Summary Information
num_observations <- nrow(new_data)
num_variables <- ncol(new_data)
max_black_jail_pop_2018 <- new_data %>%
  filter(year == "2018") %>%
  select(black_jail_pop) %>%
  max(na.rm = TRUE)
max_white_jail_pop_2018 <- new_data %>%
  filter(year == "2018") %>%
  select(white_jail_pop) %>%
  max(na.rm = TRUE)
max_total_jail_pop_2018 <- new_data %>%
  filter(year == "2018") %>%
  select(total_jail_pop) %>%
  max(na.rm = TRUE)
max_total_jail_adm_2018 <- new_data %>%
  filter(year == "2018") %>%
  select(total_jail_adm) %>%
  max(na.rm = TRUE)


# Table 
# What are the top 10 counties had the highest total jail admissions rate from 2010-2018?
library(ggthemes)
tja <- new_data %>% filter(year>=2010)  %>% 
  group_by(county_name) %>%
  summarise(
    Totaladmissions=mean(total_jail_adm_rate,na.rm=TRUE),
  ) %>%
  arrange(desc(Totaladmissions))
top10 <- tja[1:10,]

ggplot(top10) +
  geom_bar(aes(x= Totaladmissions,y = reorder(county_name, Totaladmissions)), width=0.5,stat='identity',fill="red") +
  scale_y_discrete(position = "left")+
  theme_bw(base_line_size = 0) +
  labs(title= "TOP 10 county that had the highest average total jail admissions rate from 2010-2018",y = NULL,x="average value")


# Trends over time chart
# How the jail population by race changed from 2010 to 2018 in Dickens county.?
Rp <- new_data %>% filter(county_name=="Dickens County")  %>% group_by(county_name,year) %>% summarise(
  Black=mean(black_jail_pop,na.rm=TRUE),
  Latinx=mean(latinx_jail_pop,na.rm=TRUE),
  White=mean(white_jail_pop,na.rm=TRUE)
) %>% melt(                      
  id.vars=c("county_name","year"),  
  variable.name="Race",         
  value.name="rjp"             
)
ggplot(Rp) + geom_line(aes(x = year, y =rjp,color=Race))+ facet_wrap(~county_name)+
  theme_bw(base_line_size = 1) +
  labs(title= "Average jail population by race from 2010-2018 in Dickens Country",y = "Average jail population",x="Year")


# Variable comparison chart
# How average jail population and jail admission are related?
Compare <- raw_data %>% group_by(year) %>% summarise(
  jail_population_rate=median(total_jail_pop_rate,na.rm=TRUE),
  jail_admission_rate=median(total_jail_adm_rate,na.rm=TRUE))
Compare1 <- Compare %>%melt(                      
  id.vars="year",  
  variable.name="Category",         
  value.name="Value"             
) 
ggplot(Compare1) + geom_line(aes(x = year, y =Value,color=Category))+
  theme_bw(base_line_size = 1) +
  labs(title= "Comparison of Jail Population and Jail Admission",y = "Rates",x="Year")


# Map 
# What are the white and black people jail population count in 2018 over the U.S.?
library(usmap)
library(ggplot2)
usdatawhite2018 <- new_data %>% filter(year==2018) %>% group_by(state,year) %>% summarise(
  white_jail_pop=mean(white_jail_pop,na.rm=T)
) %>% arrange(desc(white_jail_pop))
usdatawhite2018 <- usdatawhite2018[1:10,]

us2018 <- merge(usmapdata, usdatawhite2018, by.x = "abbr", by.y = "state",all.x=TRUE)
us2018[is.na(us2018)] <- 0
plot_usmap(
  data = us2018, values = "white_jail_pop", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "White people jail Population (2018)", label = scales::comma
  ) + 
  labs(title = "US Map", subtitle = "These are the top10 states that had highest white people jail population count in 2018.") +
  theme(legend.position = "right")


usdatablack2018 <- new_data %>% filter(year==2018) %>% group_by(state,year) %>% summarise(
  black_jail_pop=mean(black_jail_pop,na.rm=T)
) %>% arrange(desc(black_jail_pop))

usdatablack2018 <- usdatablack2018[2:10,] #top is DC with 1774 we remove it
us2018 <- merge(usmapdata, usdatablack2018, by.x = "abbr", by.y = "state",all.x=TRUE)
us2018[is.na(us2018)] <- 0
plot_usmap(
  data = us2018, values = "black_jail_pop", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "Black people jail Population (2018)", label = scales::comma
  ) + 
  labs(title = "US Map", subtitle = "These are the top10 states that had highest black people jail population count in 2018.") +
  theme(legend.position = "right")