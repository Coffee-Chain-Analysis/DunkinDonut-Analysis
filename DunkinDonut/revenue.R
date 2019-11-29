library(tidyverse)
library(cowplot)
library(socviz)
library(usmap)
library(RColorBrewer)

dunk_rev<- read_csv("data/dunkin.csv")

dunk_rev <- dunk_rev %>% 
  select(year_of_publish, DD_US_F:BR_IN_F, Franchise_income, Rental_income:Total_reveneu)

dunk_rev <- dunk_rev %>% 
  select(DD_US_F, DD_IN_F, BR_US_F, BR_IN_F, year_of_publish, Total_reveneu, Other_reveneue) %>% 
  gather(Type, count, DD_US_F, DD_IN_F, BR_US_F,BR_IN_F, Total_reveneu, Other_reveneue) %>% 
  na.omit()

dunk_rev <- dunk_rev%>% 
  group_by(Type, year_of_publish) %>% 
  summarise(mean = mean(count))

dunk_rev %>%
  group_by(Type) %>% 
  ggplot(mapping = aes(x = year_of_publish, y = mean)) +
  geom_line(mapping = aes(color = Type)) + ylab("Revenue") + xlab("Year")+ facet_wrap(~Type, scales = "free")

write_csv(dunk_rev, "data/dunkinrevenue#1.csv")

#----------------------------------------------------------------------------
dunk_rev<- read_csv("data/dunkin.csv")

dunk_rev <- dunk_rev %>% 
  select(year_of_publish, DD_US_F:BR_IN_F, Franchise_income, Rental_income:Total_reveneu)

dunk_rev <- dunk_rev %>% 
  select(Franchise_income, Rental_income, `Sales_of_company restaurant`, Sales_of_Icecream, year_of_publish) %>% 
  gather(Type, count, Franchise_income, Rental_income, `Sales_of_company restaurant`, Sales_of_Icecream) %>% 
  na.omit()

dunk_rev <- dunk_rev%>% 
  group_by(Type, year_of_publish) %>% 
  summarise(mean = mean(count))

dunk_rev %>%
  group_by(Type) %>% 
  ggplot(mapping = aes(x = year_of_publish, y = mean)) +
  geom_line(mapping = aes(color = Type)) + ylab("Revenue") + xlab("Year")+ facet_wrap(~Type, scales = "free")


write_csv(dunk_rev, "data/dunkrevenue#2.csv")

