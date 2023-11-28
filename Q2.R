library(tidyverse)
#read each year of the happiness report
y_2015 <- read_csv('2015.csv')
y_2016 <- read_csv('2016.csv')
y_2017 <- read_csv('2017.csv')
y_2018 <- read_csv('2018.csv')
y_2019 <- read_csv('2019.csv')


#add a year column to each dataframe and select the columns we will use 

y_2015 <- y_2015 %>% 
  mutate(Year = 2015) %>% 
  select(`Happiness Rank`, Country, `Happiness Score`,
         `Economy (GDP per Capita)`, `Health (Life Expectancy)`,
         Freedom, Generosity, `Trust (Government Corruption)`, Year)
y_2016 <- y_2016 %>% 
  mutate(Year = 2016) %>% 
  select(`Happiness Rank`, Country, `Happiness Score`,
         `Economy (GDP per Capita)`, `Health (Life Expectancy)`,
         Freedom, Generosity, `Trust (Government Corruption)`, Year)
y_2017 <- y_2017 %>% 
  mutate(Year = 2017) %>% 
  select(Happiness.Rank, Country, Happiness.Score,
         Economy..GDP.per.Capita., Health..Life.Expectancy.,
         Freedom, Generosity, Trust..Government.Corruption., Year)
y_2018 <- y_2018 %>% 
  mutate(Year = 2018) %>% 
  select(`Overall rank`, `Country or region`, Score, `GDP per capita`,
         `Healthy life expectancy`, `Freedom to make life choices`,
         Generosity, `Perceptions of corruption`, Year)
y_2019 <- y_2019 %>% 
  mutate(Year = 2019) %>% 
  select(`Overall rank`, `Country or region`, Score, `GDP per capita`,
         `Healthy life expectancy`, `Freedom to make life choices`,
         Generosity, `Perceptions of corruption`, Year)

#standardize column of the dataframes - preparing for merge
colnames(y_2015) <- colnames(y_2019)
colnames(y_2016) <- colnames(y_2019)
colnames(y_2017) <- colnames(y_2019)
colnames(y_2018) <- colnames(y_2019)

#merge dataframes
happi <- rbind(y_2015, y_2016, y_2017, y_2018, y_2019)
unique(happi$Year)

happi <- happi %>% 
  rename(Country = `Country or region`)


#read the freedom index report
free <- read_csv('hfi_cc_2020.csv')

#filter for applicable years
unique(free$year)
free <- free %>% 
  filter(year >= 2015) %>% 
  arrange(desc(year))
unique(free$year)


free <- free %>% 
  rename(Year = year,
         Country = countries)

#all columns from the freedom index
colnames(free)
#select only the columns we will use in our analysis - to cut down on variables in the analysis
free <- free %>% 
  select(Year, Country, region, hf_score, pf_rol, pf_ss_homicide, 
         pf_ss_disappearances, pf_ss_women, pf_ss, pf_movement, 
         pf_religion, pf_association, pf_expression, pf_identity, 
         ef_government, ef_legal, ef_money, ef_trade, ef_regulation)

#join happiness and freedom
happi_full <-  inner_join(happi, free, by = c('Year', 'Country')) 


happi_full <- happi_full%>% 
  rename(Happi_Score = Score) 


#Result 1

ggplot(happi_full, aes(x=`GDP per capita`, y = `Healthy life expectancy`, color=region))  + geom_point()

#Result 2

happi_full %>% 
  group_by(region) %>% 
  summarise(avgRank = mean(hf_score , is.na=FALSE), avg_Safety = mean(pf_ss_women, is.na=FALSE)) %>% 
  filter(!is.na(avg_Safety)) %>% 
ggplot( aes(x=avg_Safety, y=region))  + 
  geom_bar(fill='pink',stat="identity")+
  geom_bar(aes(x=avgRank, y=region),
           fill="gray", stat = "identity", 
           alpha=0.85, width =.51) + 
  labs(x="Average Score", y="Region", caption = "Pink denotes Women Safety, Gray denotes Happy Score")  
  

##OR## 


happi_full %>% 
  group_by(region) %>% 
  summarise(avgRank = mean(hf_score , is.na=FALSE), avg_Safety = mean(pf_ss_women, is.na=FALSE)) %>% 
  filter(!is.na(avg_Safety)) %>% 
  pivot_longer(!region, names_to = "Metric", values_to = "score") %>% 
  ggplot( aes(x=score, y=region, fill=Metric))  + 
           geom_bar( stat="identity", position="dodge")+
           labs(x="Average Score", y="Region")+ 
      scale_fill_discrete(name = "Metric", 
                          labels = c("Women Safety", "Happiness Score"))
 

 


#Result 3

ggplot(happi_full, aes(x=`Generosity`, y = `Happi_Score`, color=region))  + geom_point()