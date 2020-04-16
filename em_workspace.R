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
  group_by(ID) %>%
  mutate(avg_sent = mean(value))

# get sentiment per ~10 words
sum_df <- lex_df %>%
  group_by(ID) %>%
  group_by(ID, index = wordplace %/% 10) %>%
  summarise(sentiment = sum(value))


# the whole deal with afinn is that it rates works from -5-5 for sentiment
# look at some stuff
# NOTE: IDs for titles are inverted...most recent are lowest numbers. Consider adjusting this for plotting.

# basic blueprint for plotting absvalue sentiment value per letter
ggplot(lex_df, aes(x = ID, y = value)) + 
  geom_col()

# basic blueprint for plotting mean 
ggplot(lex_df, aes(x = ID, y = avg_sent)) + 
  geom_col()

# basic bluepring for 





