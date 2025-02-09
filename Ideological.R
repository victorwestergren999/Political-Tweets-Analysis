#############Ideological Scoring##################
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
#install.packages("stopwords")
library(stopwords)
#######Cleaning the tweets########
stop_words_regex <- paste(stopwords('en'), collapse = "\\b|\\b")
stop_words_regex <- paste0("\\b", stop_words_regex, "\\b")

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
    #remove stop words
    str_replace_all(stop_words_regex, "")%>%
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

#unnest the data
unest_data <- data %>%
  select(Tweet_ID,Handle,Party,clean_text) %>%
  unnest_tokens(word, clean_text)

#lemmatize words to their root forms
unest_data <- unest_data %>%
  mutate(stem = wordStem(word))

#stem words by tweet 
pwj <- unest_data %>%
  count(stem, Tweet_ID,Handle,Party, sort = TRUE) %>%
  group_by(Tweet_ID) %>%
  mutate(sum=sum(n),
         freq=n/sum)

#single authors words
SingAuth_word<- pwj%>%
  group_by(Handle)%>%
  count(stem)%>%
  group_by(stem)%>%
  count()%>%
  filter(n<=1)

#assign the word score to each word 
Piw <- pwj%>%
  anti_join(SingAuth_word, by="stem")%>%  #remove single authors words
  group_by(Party,stem)%>%
  summarise(Piw=mean(freq))%>%
  arrange(Piw)
  filter(Piw>=0.05&Piw<=0.95)%>%#remove words which are too freqently used or to too infrequently used
  pivot_wider(names_from = Party, values_from = Piw)%>%
  rename(prw = "Republican", pdw = "Democrat")%>%
  mutate(prw=replace(prw, is.na(prw), 0),
         pdw=replace(pdw, is.na(pdw), 0),
         sw=(prw-pdw)/(prw+pdw))

#aggregate the score at the tweet level
tweet_score <- pwj%>%
  left_join(Piw, by="stem")%>%
  mutate(score=sw*freq)%>%
  replace_na(list(score=0))%>%
  group_by(Tweet_ID)%>%
  summarise(Score=sum(score))

#normalize the score with the corpus of the republican party
score_rep_corpus <-unest_data%>%
  count(stem, Party)%>%
  group_by(Party)%>%
  mutate(sum=sum(n),
            freq=n/sum)%>%
  left_join(Piw, by="stem")%>%
  mutate(score=sw*freq)%>%
  replace_na(list(score=0))%>%
  group_by(Party)%>%
  summarise(mean_score=sum(score, na.rm = TRUE))%>%
  filter(Party=="Republican")%>%
  pull(mean_score)

#merge the score with the original data
data_score <- data %>%
  left_join(tweet_score, by="Tweet_ID")%>%
  mutate(Score=Score/score_rep_corpus)%>%
  filter(str_count(clean_text, "\\S+") > 1)#Count words and keep only tweets with more than 1 word
  



#plot the histogram of the score
ggplot(data_score, aes(x = Score, fill = Party)) +
  geom_density(alpha = 0.8) +
  xlim(-10,10)+
  theme_minimal() +
  labs(title = "Density plot of the ideological score", x = "Score", y = "Density") +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +  # Blue for Dems, Red for Reps
  facet_wrap(~Party, ncol = 1)  # Stack facets vertically

