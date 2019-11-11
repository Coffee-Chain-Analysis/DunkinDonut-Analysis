library(tidyverse)
library(usmaps)
library(cowplot)
library(socviz)
library(usmap)
df<- read_csv("DD-US.csv",col_names = FALSE)
zip<-read_csv("ZIP-COUNTY-FIPS-2018.csv")
cmap<-county_map
us_states<-map_data("state")

zip<-zip %>% 
  mutate(ZIP=as.numeric(ZIP))

df<-df %>% 
  mutate(ZIP=as.numeric(str_match(X4,"(\\d{5})")[1]))

data<-left_join(zip,df,by="ZIP") %>% 
  mutate(COUNTYNAME=str_remove(COUNTYNAME," County")) %>% 
  rename(subregion=COUNTYNAME) %>% 
  mutate(subregion=tolower(subregion)) %>% 
  group_by(subregion) %>% 
  count() %>% 
  na.omit() 


cmp<-map_data("county")
data<-left_join(cmp,data,by='subregion')

data %>%
  ggplot() +
  # geom_point(mapping = aes(x=long,y=lat)
  geom_polygon(mapping = aes(x=long,y=lat,group=group,fill=n,color="white")) +
  scale_fill_distiller(palette = "Paired") +
  geom_point(data=df,mapping = aes(x=X1,y=X2),size=0.5,alpha=0.3,color="red")+
  coord_equal()+
  theme_map()+
  guides(fill = guide_legend())+
  theme(panel.background = element_blank())
  # facet_wrap(~region)

#-------------------------------------------------------------------------------------------------------------------
#state


data<-left_join(zip,df,by="ZIP") %>% 
  mutate(COUNTYNAME=str_remove(COUNTYNAME," County")) %>% 
  rename(subregion=COUNTYNAME) %>% 
  mutate(subregion=tolower(subregion)) %>% 
  group_by(STATE) %>%
  count() %>%
  na.omit()



us_states<-us_map("states") %>% 
  rename(STATE=abbr)

data<-left_join(us_states,data,by='STATE')

data %>% 
  ggplot() +
  # geom_point(mapping = aes(x=long,y=lat)
  geom_polygon(mapping = aes(x=x,y=y,group=group,fill=n)) +
  scale_fill_distiller(palette = "Set1") +
  # geom_point(data=df,mapping = aes(x=X1,y=X2),size=1,alpha=0.3,color="red")+
  coord_equal()+
  theme_map()+
  theme(panel.background = element_blank())
# facet_wrap(~region)


