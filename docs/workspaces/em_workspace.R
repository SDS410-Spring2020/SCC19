library(dplyr)
library(stringr)
library(textdata)
library(tidytext)
 
# load in data from repo
load("textdata.Rda")

# clean data, get words alone for tidytext (stolen from Jeny...thanks Jeny!)
df_tidy <- df_full %>% 
  unnest_tokens(word, content) %>% 
  anti_join(stop_words)

df_tidy <- df_tidy %>%
  group_by(ID) %>% 
  mutate(wordplace = row_number()) %>% 
  ungroup()

# get afinn lexicon into df
afinn_lex <- get_sentiments("afinn")

# join afinn to tidy text dataset
lex_df <- inner_join(df_tidy, afinn_lex, by = "word") %>%
  group_by(ID, index = wordplace %/% 10) %>%
  mutate(avg_sent = mean(value))

# get sentiment per ~10 words
sum_df <- lex_df %>%
  group_by(date, ID) %>%
  #group_by(date, index = wordplace %/% 10) %>%
  summarise(sentiment = sum(value), avg_sent = mean(value))


# the whole deal with afinn is that it rates works from -5-5 for sentiment
# look at some stuff
# NOTE: IDs for titles are inverted...most recent are lowest numbers. Consider adjusting this for plotting.

# basic blueprint for plotting sentiment over course of letters (faceted)
ex_df <- lex_df %>%
  filter(ID %in% c(1:5))

ggplot(ex_df, aes(x = wordplace, y = value)) + 
  geom_col() + 
  facet_wrap(~ID)

# get data for length of each letter individually (number of not filler words)
letter_length <- lex_df %>%
  group_by(date) %>%
  summarise(length = max(wordplace), avg_value = mean(value))


# what do we want for this analysis? 
# length of letters over time
ggplot(letter_length, aes(x = date, y = length)) + 
  geom_line() + 
  theme_light() + 
  labs(x = "Date", y = "Length of Letter")

# sentiment of letters over time (total and average)
ggplot(sum_df, aes(x = date, y = sentiment)) + 
  geom_line() + 
  theme_light() + 
  labs(x = "Date", y = "Total Sentiment")
# basic blueprint for plotting mean 
ggplot(sum_df, aes(x = date, y = avg_sent)) + 
  geom_line() + 
  theme_light() + 
  labs(x = "Date", y = "Average Sentiment")
# identify points of excessive conflict or negativity
# identify points of excessive positivity









