######## Complexity Measure##########

#load libraries
#install.packages(quanteda.textstats)
library(quanteda.textstats)
#install.packages(tidyverse)
library(tidyverse)
#install.packages("SnowballC")
library(SnowballC)
#install.packages("readr")
library(readr)
#install.packages("tidytext")
library(tidytext)
#install.packages("stringr")
library(stringr)

#Clean tweets 
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
    str_remove_all("[[:punct:]]") %>%
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
#load the dataset
data <-read.csv("~/Downloads/ExtractedTweets.csv", comment.char="#")
#create tweet_id
data$Tweet_ID <- seq.int(nrow(data))
#clean tweets
data <- data%>%
  mutate(clean_text = clean_tweets(Tweet))

#convert the tweet dataset to corpus
tweet_corpus <- corpus(data$clean_text,docnames = data$Tweet_ID)
#compute the readability scores
readability_scores <- textstat_readability(
  tweet_corpus ,
  measure = "Flesch.Kincaid"
)

#convert the readability scores to a dataframe
readability_scores <- readability_scores %>%
  mutate(document = as.integer(document))
#merge the readability scores with the original data
data <- data %>%
  left_join(readability_scores, by = c("Tweet_ID" = "document"))

#plot the density of the sentiment score
ggplot(data, aes(x = Flesch.Kincaid, fill = Party)) +
  geom_density(alpha = 0.8) +
  #xlim(-10,10)+
  theme_minimal() +
  labs(title = "Density plot of the Flesch Kincaid score", x = "Score", y = "Density") +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +  # Blue for Dems, Red for Reps
  facet_wrap(~Party, ncol = 1)  # Stack facets vertically

#summarise
data %>%
  group_by(Party) %>%
  summarise(mean_Flesch_Kincaid = mean(Flesch.Kincaid, na.rm = TRUE),
            sd_Flesch_Kincaid = sd(Flesch.Kincaid, na.rm = TRUE),
            n = n())
#t.test scores by party 
t.test(Flesch.Kincaid ~ Party, data = data)


