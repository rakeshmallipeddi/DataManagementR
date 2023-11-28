library(tidyverse)
library(stringr)
cyber <- read_csv("cyber11052019.csv", col_names = T, col_types = c("user_id"="character", "status_id"="character", "retweet_status_id"="character"))

glimpse(cyber)


#************************
#*#Question 1 
#*#************************

#removing data with no hashtags and converting text in hastags column to uppercase
df1 <- 
  cyber %>% 
  select(user_id, hashtags) %>% 
  mutate(hashtags = str_to_upper(hashtags)) %>%
  filter(!is.na(hashtags)) %>% 
  arrange(user_id)

#Removing extra white-space
df1$hashtags <- str_squish(df1$hashtags)

#creating a new row for each hastag
df2 <- 
  df1 %>% 
  separate_longer_delim(hashtags,  delim = ",")


#replacing non-alphanumeric strings with space
df2 <-
  df2 %>% 
  mutate(newhashtags = str_replace_all(hashtags, "[^[:alnum:]]", " "))


#removing all additional white spaces
df2$hashtags <- str_trim(df2$newhashtags, side="both")

#counting the number of times each hashtag shows up in the data and using filter to keep only those which appear more than 50 times. 
df3 <- 
  df2 %>% 
  group_by(hashtags) %>%
  summarize(count=n()) %>% 
  filter(count>=50) %>% 
  arrange(desc(count))

#code for treemap
library(treemapify)

ggplot(df3, aes(area = count, fill=count,  label = hashtags)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", min.size=2, colour = "white", place = "centre", grow = TRUE) 

#************************
#* Question 2
#*************************

#converting to date format
cyber$newDate <- ymd_hms(cyber$created_at)

#extracting month, year, and hour
cyber$month <- month(cyber$newDate)
cyber$year <- year(cyber$newDate)
cyber$hour <- hour(cyber$newDate)

#counting number of tweets per hour and mean retweet_count
Q2Report <-
  cyber %>% 
  group_by(hour) %>% 
  summarise(nTweets = n(), MeanRT = mean(retweet_count, is.na=T))

#counting number of tweets per hour and mean retweet count only for original tweets. 
Q2Reporta <-
  cyber %>% 
  group_by(hour) %>% 
  filter(is_retweet=="FALSE") %>% 
  summarise(nTweets = n(), MeanRT = mean(retweet_count, is.na=T))


#************************
#* Question 3
#*************************
#Using summarize function get the most popular source for tweeting. 

Q3Report <-
  cyber %>% 
  group_by(source) %>% 
  summarise(nsource = n()) %>% 
  arrange(desc(nsource))


#************************
#* Question 4
#*************************

#Extracting text that starts with @
Q4a <- 
  cyber %>% 
  filter(is_retweet=="FALSE") %>% 
  select(status_id, text) %>% 
  mutate(tags =  str_extract_all(text, "@\\S+")) %>% 
  mutate(tags2 = paste(tags))


#clean up the tags column

Q4a <-
  Q4a %>% 
  mutate(allTags = str_replace_all(tags2, ",", " " )) %>% 
  mutate(allTags = str_replace_all(allTags, '"' , " "  )) %>% 
  mutate(allTags = str_replace_all(allTags, "[)]", " ")) %>% 
  mutate(allTags = str_replace_all(allTags, "c[(]", " "))



#remove white spaces
Q4a <-
  Q4a %>% 
  mutate(allTags = str_squish(allTags)) %>% 
  filter(tags != "character(0)")

#remove "@"
Q4a <-
  Q4a %>% 
  mutate(allTags = str_replace_all(allTags, "@"," "))


Q4a <- 
  Q4a %>% 
  separate_longer_delim(allTags,  delim = " ") %>% 
  select(allTags,tags) %>% 
  filter(allTags !="")
  

#Counting number of times a person was mentioned. 
Q4b <- 
  Q4a %>% 
  group_by(allTags) %>%
  summarize(count=n()) %>% 
  arrange(desc(count)) %>% 
  filter(count>=3)

library(treemapify)

ggplot(Q4b, aes(area = count, fill=count,  label = allTags)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", min.size=2, colour = "white", place = "centre", grow = TRUE) 



 