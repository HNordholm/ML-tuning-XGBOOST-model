#        EXPLORATORY DATA ANALYSIS --- 

# Number of views will be treated as demand / popularity in this analysis. 
# Exploring relationships to that feature will be of interest. -- 

# Is there any relationship between price and number of views ? 

boat %>% 
  ggplot(aes(log(price_eur),number_of_views_last_7_days))+
  geom_point(shape=19,size=0.6,color="midnightblue",alpha=0.6)+
  labs(title="N of views vs. price",
       y="Number of views(last 7 days)",
       x="lg(price)")

# Is older boats more or less expensive ? 

boat %>% 
  filter(year_built>1900) %>% 
  ggplot(aes(year_built,price_eur))+
  geom_point(shape=19,size=3,alpha=0.1,color="midnightblue")+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Price by time",y=NULL,x=NULL)


#Avg number of views per boat in Countries -- 

boat %>% 
  group_by(country) %>% 
  summarise(avg_views=mean(number_of_views_last_7_days)) %>% 
  ggplot(aes(avg_views,fct_reorder(country,avg_views),fill=country))+
  geom_col()+
  theme(legend.position="NONE")+
  labs(title="AVG views per boat",y=NULL,x=NULL)

# N of views by type -- 
boat %>% 
  ggplot(aes(number_of_views_last_7_days,
             fct_reorder(type,number_of_views_last_7_days),
             fill=type))+
  geom_boxplot()+
  theme(legend.position ="NONE")+
  labs(title="Views by type",x=NULL,y=NULL)

#N of views by boat_type -- 

boat %>% 
  ggplot(aes(number_of_views_last_7_days,
             fct_reorder(boat_type,number_of_views_last_7_days),
             fill=boat_type))+
  geom_boxplot()+
  theme(legend.position ="NONE")+
  labs(title="Views by boat_type",x=NULL,y=NULL)