#############Sentimental Analysis##################
#libraries
library(tidyverse)
#install.packages("SnowballC")
library(SnowballC)
#install.packages("readr")
library(readr)
#install.packages("tidytext")
library(tidytext)
#install.packages("stringr")
library(stringr)
#install.packages("syuzhet")
library(syuzhet)


clean_tweets <- function(x) {
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove "RT: " from beginning of retweets
    str_remove_all("^RT @[a-z,A-Z]*[0-9]*[a-z,A-Z]*[0-9]*:") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}")%>%
    # Remove hashtags
    str_remove_all("#") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove puntucation, using a standard character class
    #str_remove_all("[[:punct:]]") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove any trailing whitespace around the text
    str_trim("both")%>%
    #remove numbers
    str_remove_all("[0-9]")
}

#the data can be accessed from the following link
#https://www.kaggle.com/datasets/kapastor/democratvsrepublicantweets?resource=download
#load the data 
data <-read.csv("~/Downloads/ExtractedTweets.csv", comment.char="#")
#create tweet_id
data$Tweet_ID <- seq.int(nrow(data))

#clean tweets
data <- data%>%
  mutate(clean_text = clean_tweets(Tweet))

tweets_sentiment <- data%>%
  mutate(sentiment = get_sentiment(clean_text,method = "syuzhet"),
         positive= ifelse(sentiment>0,1,0))


#plot the density of the sentiment score
ggplot(tweets_sentiment, aes(x = sentiment, fill = Party)) +
  geom_density(alpha = 0.8) +
  xlim(-10,10)+
  theme_minimal() +
  labs(title = "Density plot of the sentimal score", x = "Score", y = "Density") +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +  # Blue for Dems, Red for Reps
  facet_wrap(~Party, ncol = 1)  # Stack facets vertically
  