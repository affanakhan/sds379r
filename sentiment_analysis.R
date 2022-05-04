#install all relevant packages
#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("textdata")
#install.packages('gridExtra') 
#install.packages("highcharter")
#install.packages("viridis")
#install.packages("car")

#open all relevant libraries
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tidytext)
library(stringr)
library(tidyr)
library(gridExtra)
library(highcharter)
library(viridis)

#open all tweet csv files
tweets_output1_elon <- read_csv("desktop/sds379/output1.csv")
tweets_output2_ukraine <- read_csv("desktop/sds379/output2.csv")
tweets_output3_covid <- read_csv("desktop/sds379/output3.csv")
tweets_output4_oscars <- read_csv("desktop/sds379/output4.csv")
happy_index_ranking <- read_csv("desktop/sds379/happiness_index.csv")

########### PREPROCESSING AND UNDERSTANDING DATASET

#map: National Happiness Index per Country

happy_index_ranking

hcmap(
  "custom/world-robinson-lowres", 
  data = happy_index_ranking,
  name = "happiness2021", 
  value = "happiness2021",
  borderWidth = 0,
  nullColor = "#d3d3d3",
  joinBy = c("name", "country")
) %>%   
  hc_title(text = "National Happiness Index per Country") %>% 
  hc_subtitle(text = "Derived from the World Happiness Report 2021") %>% 
  hc_colorAxis(
    stops = color_stops(colors = viridisLite::inferno(10, begin = 0.1)),
    type = "logarithmic"
  ) 

head(tweets_output1_elon, 5)

#combine tweet counts
tweet_count_elon <- c(tweets_output1_elon %>% 
                    count(country_name))
tweet_count_ukraine <- c(tweets_output2_ukraine %>% 
                    count(country_name))
tweet_count_covid <- c(tweets_output3_covid %>% 
                    count(country_name))
tweet_count_oscars <- c(tweets_output4_oscars %>% 
                    count(country_name))
df_tweet_counts <- data.frame(tweet_count_elon, tweet_count_ukraine, tweet_count_covid, tweet_count_oscars)
#fix and rename df
drop <- c("country_name.1","country_name.2", "country_name.3")
df_tweet_counts = df_tweet_counts[,!(names(df_tweet_counts) %in% drop)]
df_tweet_counts <- df_tweet_counts %>% 
  rename(
    elon_count = n,
    ukraine_count = n.1,
    covid_count = n.2,
    oscars_count = n.3
  )
#tweet count by country
df_tweet_counts
write.csv(df_tweet_counts,"desktop/sds379/count.csv", row.names = FALSE)

#get updated tweet count
df_tweet_counts_updated <- read_csv("desktop/sds379/count_updated.csv")
df_tweet_counts_updated

#get grouped bar chart based on topic 
df_tweet_counts_updated %>% 
  hchart('column', hcaes(x = 'country_name', y = 'count', group = topic )) %>%    
  hc_title(text = "Tweets per Country by Current Event Issue") %>% 
  hc_subtitle(text = "Derived from Twitter") %>%  
  hc_yAxis(title = list(text = "Count")) %>%
  hc_xAxis(title = list(text = "Countries")) %>%
  hc_colors(colors = viridisLite::inferno(4, begin = 0.1))

#get total tweet information and counts 

df_tweet_counts <- df_tweet_counts %>% rowwise() %>%
  mutate(total_count = sum(c_across(elon_count:oscars_count)))
print(sum(df_tweet_counts$total_count))

#get bar chart based total tweets
df_tweet_counts %>% 
  hchart('column', hcaes(x = 'country_name', y = 'total_count')) %>%    
  hc_title(text = "Total Tweets per Country") %>% 
  hc_subtitle(text = "Derived from Twitter") %>%  
  hc_yAxis(title = list(text = "Count")) %>%
  hc_xAxis(title = list(text = "Countries")) %>%
  hc_colorAxis(
    stops = color_stops(colors = viridisLite::inferno(10, begin = 0.1)),
    type = "logarithmic"
  ) 

########### SENTIMENT ANALYSIS - AFINN Sentiment

afinn <-  tweets_output4_oscars %>% 
  filter(!str_detect(tweet_text, "^RT")) %>%
  mutate(tweet_text = str_remove_all(tweet_text,  "#[a-z,A-Z]*|@[a-z,A-Z]")) %>%
  unnest_tokens(word, tweet_text, token = "tweets") %>%
  filter(!word %in% s$word,
         !word %in% str_remove_all(s$word, "'"),
         str_detect(word, "[a-z]")) <- assess_sentiment %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(country_name, index = index_v) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

ggplot(afinn, aes(index, sentiment, fill = country_name)) +
  geom_col(show.legend = FALSE) + labs(title="afinn Sentiment Index per Tweet by Country (oscars)", x ="Number of Tweets", y = "afinn Sentiment Index") +
  scale_fill_viridis(discrete=TRUE, option="inferno") + facet_wrap(~country_name, ncol = 5, scales = "free_x") + theme(plot.title = element_text(hjust = 0.5))

afinn_elon <-afinn
afinn_elon

afinn_ukraine <-afinn
afinn_ukraine

afinn_covid <-afinn
afinn_covid

afinn_oscars <-afinn
afinn_oscars

#get grouped bar chart based on topic 
afinn %>% 
  hchart('column', hcaes(x = 'country_name', y = 'sentiment', group = country_name )) %>%    
  hc_title(text = "Tweets per Country by Current Event Issue") %>% 
  hc_subtitle(text = "Derived from Twitter") %>%  
  hc_yAxis(title = list(text = "Count")) %>%
  hc_xAxis(title = list(text = "Countries")) %>%
  hc_colors(colors = viridisLite::inferno(10, begin = 0.1))

########### SENTIMENT ANALYSIS - Bing words

assess_sentiment %>%
  count(word, sentiment, sort = TRUE) %>%
  inner_join(get_sentiments("bing")) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  slice_max(n, n = 20) %>% 
  mutate(word = reorder(word, n)) %>% 
  geom_col(show.legend = FALSE)
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Percent Contribution to Sentiment Index (oscars)",
  y = NULL) + scale_fill_viridis(discrete=TRUE, option="inferno")+
  ggplot(aes(n, word, fill = sentiment)) 

########### Statistical Analysis 

#mean of elon
mean_afinn_elon<-afinn_elon %>%
  group_by(country_name) %>% 
  summarise(num = n(),
            total_index = mean(sentiment))
mean_afinn_elon

#mean of ukraine
mean_afinn_ukraine<-afinn_ukraine %>%
  group_by(country_name) %>% 
  summarise(num = n(),
            total_index = mean(sentiment))
mean_afinn_ukraine

#mean of covid
mean_afinn_covid<-afinn_covid %>%
  group_by(country_name) %>% 
  summarise(num = n(),
            total_index = mean(sentiment))
mean_afinn_covid

#mean of oscars
mean_afinn_oscars<-afinn_oscars %>%
  group_by(country_name) %>% 
  summarise(num = n(),
            total_index = mean(sentiment))
mean_afinn_oscars

#write out the files
write.csv(mean_afinn_elon,"desktop/sds379/index_elon.csv", row.names = FALSE)
write.csv(mean_afinn_ukraine,"desktop/sds379/index_ukraine.csv", row.names = FALSE)
write.csv(mean_afinn_covid,"desktop/sds379/index_covid.csv", row.names = FALSE)
write.csv(mean_afinn_oscars,"desktop/sds379/index_oscars.csv", row.names = FALSE)

#open agg table
sent_table <- read_csv("desktop/sds379/sent_table.csv")
hchart(sent_table, "point", hcaes(x = happiness2021, y = sent_index, group = topic))

library(dplyr)
library(broom)
model <- lm(sent_index ~ happiness2021, data = sent_table)
fit <- augment(model) %>% arrange(happiness2021)

#sent index vs happiness table
sent_table %>% 
  hchart('scatter', hcaes(x = happiness2021, y = sent_index, group = topic)) %>%    
  hc_title(text = "afinn Sentiment Index VS Happiness Index") %>% 
  hc_subtitle(text = "Derived from Twitter and World Happiness Report 2021") %>%  
  hc_yAxis(title = list(text = "afinn Sentiment Index")) %>%
  hc_xAxis(title = list(text = "Happiness Index")) %>%
  hc_colors(colors = viridisLite::inferno(4, begin = 0.1)) %>% 
  hc_add_series(
    fit, type = "line", hcaes(x = happiness2021, y = .fitted),
    name = "Logistic Regression", id = "fit"
  ) 

########### ANOVA Tests

dat <- data_to_boxplot(sent_table, sent_index, country_name, name = "afinn_sentiment_index")

highchart() %>%
  hc_xAxis(type = "category") %>% d 
  hc_add_series_list(dat)    %>% 
  hc_title(text = "afinn Sentiment Index per Country") %>% 
  hc_subtitle(text = "Derived from Twitter") %>%  
  hc_yAxis(title = list(text = "afinn Sentiment Index")) %>%
  hc_xAxis(title = list(text = "Country Name")) %>%
  hc_colors(colors = viridisLite::inferno(20, alpha = 1, begin = 0, end = 1, direction = 1))


sent_ag <- read_csv("desktop/sds379/sent_ag.csv")
sent_ag
my_anova <- lm(sent_index ~ country_name, data = sent_table)
library(car)

leveneTest(sent_index ~ country_name, data = sent_table)


Anova(my_anova, type = 3)

summary(my_anova)$r.squared

install.packages("emmeans")
library(emmeans)

emmeans(my_anova, pairwise ~ country_name)


########### Linear Regression

# Calculate Pearson correlation
cor(sent_table$sent_index, sent_table$happiness2021)
cor.test(sent_table$sent_index, sent_table$happiness2021)

#  Linear Regression 
model_m <- lm(sent_index ~ happiness2021, data = sent_table)
qqnorm(model_m$residuals, main = "QQ-plot of Model Residuals")
qqline(model_m$residuals, col = "red")

# Confirm equal variance
plot(model_m$fitted.values,  main = "Plot of Variance", model_m$residuals, xlab = "Fitted Values", 
     ylab = "Residuals", pch = 20)
abline(h = 0, col = "red")

summary(model_m)

plot(sent_table$happiness2021, sent_table$sent_index, main = "afinn Sentiment Index VS Happiness Index", 
     xlab = "Happiness Index", ylab = "afinn Sentiment Indext", pch = 20)
abline(model_m, col = "blue")


#Sources:
#"Text Mining with R: A Tidy Approach" was written by Julia Silge and David Robinson. It was last built on 2022-05-03.
#https://www.highcharts.com/blog/tutorials/highcharts-for-r-users/ 
#https://www.tidytextmining.com/sentiment.html 

