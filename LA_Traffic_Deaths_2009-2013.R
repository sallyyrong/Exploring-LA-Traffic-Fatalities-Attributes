#2009-2013 LA Traffic-related Fatalities & Attributes: A Surface Level Examination
#authors: Sally Rong, Elena Castellanos
#08/06/2020

#rm(list=ls()) 
#setwd("~/Desktop/Data Science") 
#install.packages('tidyverse')

# Load the data
library(tidyverse)
collisions2 <- read_csv('Collisions_2009-2013_(SWITRS).csv')
party_table <- read_csv('Party_Tables_-_Collisions_2009-2013_(SWITRS).csv')
victim_table <- read_csv('Victim_Tables_-_Collisions_2009-2013_(SWITRS).csv')

# PARTY-VICTIM relationship
#Merge party_table & victim_table, by CASE ID and any number 
#of party records that can be associated w/ a collision
party_victim <- left_join(x=party_table, y=victim_table, by= c("CASE_ID", "PARTY_NUMBER"))
dim(party_victim)

# COLLISION-PARTY relationship
#For this join, we faced difficulting tying the two datasets by CASE_ID. We have ran 
#this problem through with Professor Raja too.
# collision_party <- left_join(x=collisions2, y=party_table, by= c("CASE_ID"))

df <- party_victim %>%
  select(PARTY_AGE, PARTY_SEX, VICTIM_DEGREE_OF_INJURY, VICTIM_SAFETY_EQUIP_1, VICTIM_ROLE, VICTIM_AGE, VEHICLE_YEAR, VICTIM_SEX, STWD_VEHICLE_TYPE, VEHICLE_MAKE)
dim(df)

# We first looked only at fatal accidents.
fatal <- filter(df, VICTIM_DEGREE_OF_INJURY == 4)
dim(fatal)

fatal %>%
  group_by(VICTIM_SAFETY_EQUIP_1) %>%
  summarize(num = n()) %>%
  arrange(desc(num)) %>%
  mutate(percentage = num/277934)
#It appears that in accidents that resulted in the victim killed,  about 46.3% of those 
#accidents involved the air bag not deployed ('M' above). 

#Here's a graph showing the Victim Safety Equipment used in fatal traffic accidents. Most 
#frequently occuring is the air bag not deployed (M).
fatal %>%
  ggplot(mapping = aes(x=VICTIM_SAFETY_EQUIP_1)) +
  geom_histogram(
    stat = 'count') +
  labs(y = 'Frequency Count',
       x= 'Victim Safety Equipment',
       title = 'Traffic Fatalies and Saftey Equipment Involved')

no_air_bag_fatal <- filter(df, VICTIM_SAFETY_EQUIP_1 == 'M', VICTIM_DEGREE_OF_INJURY == 4)
dim(no_air_bag_fatal)
#128730/277934 (about 46%) of traffic accidents in which victim was killed involved air bag not deployed.


# We then looked at the most frequent ages of parties involved in fatal accidents in which the air bag was not deployed.
no_air_bag_fatal %>%
  filter(PARTY_AGE != 998) %>%
  group_by(PARTY_AGE) %>%
  summarize(number_parties_in_fatal_accidents = n()) %>%
  arrange(desc(number_parties_in_fatal_accidents))
#Younger parties in their twenties are more frequent in fatal traffic accidents of no air bag deployment.

no_air_bag_fatal %>%
  filter(PARTY_AGE != 998 ) %>%
  ggplot(mapping = aes(x=PARTY_AGE, fill = PARTY_SEX, color= PARTY_SEX)) +
  geom_histogram(
    stat = "count",
    bin = 0.5) +
  labs(y = 'Frequency Count',
       x= 'Party Age',
       title = 'Frequency of Parties Involved in Fatal Traffic Accidents of No Air Bag Deployment by Age')

filter (no_air_bag_fatal) %>%
  group_by(VEHICLE_MAKE) %>%
  summarize(num = n()) %>%
  arrange(desc(num))
# Of the fatal accidents without the airbag deployed, these are the top car makes.
no_air_bag_fatal %>%
  filter( VEHICLE_MAKE %in% c('TOYOTA', 'HONDA', 'FORD',  'NISSAN', 'CHEVROLET')) %>%
  ggplot(mapping = aes(x=VEHICLE_MAKE, fill = VEHICLE_YEAR, color= VEHICLE_YEAR)) +
  geom_histogram(
    stat = 'count') +
  theme_light() +
  labs(y = 'Frequency Count',
       x= 'Car Makes',
       title = 'Top 5 Car Makes in Fatal No-Airbag-Deployed Accidents')
# Interestingly, some car companies had to recall their cars for defective airbags (more info in the links below). The companies 
#with most cars with the defective Takata airbags were also Honda, Ford, and Toyota.
#https://thomasjhenrylaw.com/blog/product-liability/takata-airbag-recall-a-complete-list-of-affected-vehicles/
#https://www.consumerreports.org/car-recalls-defects/takata-airbag-recall-everything-you-need-to-know/]
#https://www.newsweek.com/airbag-recall-list-car-models-toyota-ford-honda-bmw-nissan-takata-1483468

no_air_bag_fatal %>%
  group_by(VEHICLE_YEAR) %>%
  summarize(num = n()) %>%
  arrange(desc(num))
# Of Vehicles involved in fatal accidents of no air bag deployment, these are the top car years of vehicles involved. This dataset 
#doesn't include information on specific car models, though the articles do point out the top car make and models involved in the Takata air bag recall. 




# Now we will examine fatal accidents including bicyclists only. 
bicyclist_fatal <- filter(df, VICTIM_ROLE == 4 & VICTIM_DEGREE_OF_INJURY == 4)
dim(bicyclist_fatal)

bicyclist_fatal %>%
  filter(VICTIM_SAFETY_EQUIP_1 != '-' ) %>%
  group_by(VICTIM_SAFETY_EQUIP_1) %>%
  summarize(num = n()) %>%
  arrange(desc(num))
# Most frequent was P - not required. We didn't evaluate that, since the codebook wasn't clear on what that means. We looked at V instead, the 2nd most 
#frequent - 'Driver, Motorcycle Helmet not Used.' We also included X - 'Passenger, motorcycle, Helmet not used' for a more robust look at cases that the helmet was not used. 


bicyclist_fatal %>%
  filter(VICTIM_SAFETY_EQUIP_1 %in% c('V', 'X')) %>%
  group_by(VICTIM_AGE) %>%
  summarize(num = n()) %>%
  arrange(desc(num))
# Again, it seems bicyclists in at very young ages, 18-20 years, appear the most frequent in these fatal accidents in which helmets aren't used. 
# Bicyclists over the age of 18 are not required to wear helmets. Perhaps deeper analysis may provide information whether there is correlation between these two factors.
# https://www.bicyclelaw.com/bicycle-laws/california-bicycle-laws/california-bicycle-helmet-law/ 

bicyclist_fatal %>%
  filter(VICTIM_SAFETY_EQUIP_1 %in% c('V', 'X') & VICTIM_AGE != 998) %>%
  ggplot(mapping = aes(x=VICTIM_AGE, fill = VICTIM_SEX, color= VICTIM_SEX)) +
  geom_histogram(
    stat = "count",
    bin = 0.5) +
  labs(y = 'Frequency Count',
       x= 'Bicyclist Age',
       title = 'Frequency of Bicyclist Victims Involved in Fatal Traffic Accidents without Helmet')

