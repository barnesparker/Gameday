library(tidyverse)
library(DataExplorer)
filename <- "raw_tweet_data.csv"
raw_tweet_data <- filename %>% read_csv()
tweet_data <- raw_tweet_data %>% 
  select(`Ad Name`, created_at, entities, id, in_reply_to_screen_name, 
         lang, possibly_sensitive,
         Sheet, text, user, favorite_count, quote_count, reply_count, retweet_count) %>% 
  rename(ad_name = `Ad Name`)
tweet_data %>% 
  group_by(ad_name) %>% 
  summarise(total = n()) %>% 
  select(ad_name, total) %>% 
  filter(total > 5000) %>% 
  arrange(total) %>% 
  ggplot(aes(reorder(ad_name, total), total)) +
  geom_col() +
  coord_flip()

tweet_data %>% 
  group_by(lang) %>% 
  summarise(total = n()) %>% 
  select(lang, total) %>% 
  arrange(total) %>% 
  ggplot(aes(reorder(lang, total), total)) +
  geom_col() +
  coord_flip()

tweet_data %>% plot_missing(missing_only = T)

ad_stats <- tweet_data %>% 
  group_by(ad_name) %>% 
  summarise(total_tweets = n(),
            total_favs = sum(favorite_count),
            total_rts = sum(retweet_count),
            total_replies = sum(reply_count),
            total_quotes = sum(quote_count),
            total_engagements = sum(c(total_favs, total_replies, total_rts, total_quotes)))

ad_stats %>% 
  ggplot(aes(reorder(ad_name, -total_engagements), total_engagements)) +
  geom_col() +
  coord_flip()

### Which brands garnered the most popular tweets (top 1%)) ###
top <- tweet_data %>% 
  # rowwise() %>% 
  # mutate(total_engagement = sum(c(favorite_count, retweet_count, reply_count, quote_count))) %>%
  
  # ungroup() %>% 
  slice_max(order_by = favorite_count, prop = .01) %>%
  group_by(ad_name) %>% 
  summarise(top_tweets = n()) %>% 
  arrange(desc(top_tweets)) %>% 
  group_by(ad_name) %>% 
  summarise(top_tweets = sum(top_tweets))

pie_data <- top %>% 
  # mutate(ad_name = if_else(top_tweets %in% (top[1:10,2] %>% pull()), ad_name, "Other Ads")) %>%
  group_by(ad_name) %>% 
  summarise(top_tweets = sum(top_tweets)) %>% 
  slice_max(order_by = top_tweets, n = 10) %>% 
  mutate(prop = round(top_tweets / sum(top_tweets), 2),
         ypos = cumsum(prop) - .5 * prop)
         

pie_data %>% 
  ggplot(aes(x=1, y=prop, fill = ad_name)) +
  geom_bar(stat = 'identity', width = 1, color = "white") +
  geom_text(aes(y = ypos, label = ad_name), color = "white", size=4) +
  labs(title = "Which ads were the top 1% most popular tweets about?")



