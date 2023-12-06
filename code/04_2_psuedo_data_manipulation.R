rm(list = setdiff(ls(), c("NAICS2", "data", "THE_SDG_CATEGORY")))
library(tidyverse)
library(tidytext)
library(ggplot2)
source("./code/utils/func.R",encoding = "")
source("./code/utils/weakWords.R")

#df_final_key 如果要用SDSN 的Keyword改這邊
# TODO: pick up a set of keywords to use
df_final_key <- read_rds("./data/cleaned_data/df_final_key_all.rds")
# df_final_key <- read_rds("./data/cleaned_data/df_final_key.rds")
df.RankCode <- getRankCodeMap("./data/raw_data/TM Final_FortuneG500 (2021)_v2.xlsx")

# Load the NAICS code you want
# We have name, rank, year, value (the content of the report) in the following datafram
df.doc <- readReports(NAICS2_CODE = NAICS2)


# Here we tokenize the content of the report 
df.word <- df.doc %>% 
  unnest_tokens(output = word, input = value, token = "words") %>% 
  filter(!word %in% stopwords::stopwords()) %>% 
  filter(!word %in% base::letters) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "\\d+\\w")) %>% 
  filter(!str_detect(word, "\\d+")) %>% 
  filter(!word %in% weak_words)

# Here we compute the total number of words used in the reports for a firm within a year
# this number is going to be the denominator
df.wordlen <- df.word %>%
  group_by(year, rank) %>%
  count(name) %>%
  mutate(name = str_replace(name, "\\d+\\s", "")) %>% 
  ungroup()
df.wordlen <- df.wordlen %>% 
  mutate(name = str_remove(name, "_20\\d{2}")) %>% 
  group_by(rank, name) %>% 
  summarise(n = sum(n)) %>% 
  ungroup()


# Join two dataframes from script 1 and 2
df.word %>% head()
df_final_key %>% head()
# TODO: make sure the names of firms are aligned
df.wordCount <- read_rds(paste0("./data/cleaned_data/df_wordCount_NAICS", NAICS2, ".rds"))

df.combine <- df.wordCount %>%
  left_join(df_final_key, by = c("keyword" = "word")) %>% 
  mutate(year = as.numeric(str_extract(name, "20\\d+")),
         name = str_replace(name, "_20\\d+", ""),
         rank = as.numeric(str_extract(name, "\\d+")),
         name = str_replace(name, "\\d+\\s", ""))


## Find the numerator regardless of year
## the group's primary key is firm's name and SDG category
df.long <- df.combine %>% 
  filter(n_keyword > 0) %>%
  group_by(rank, name, sdg) %>% 
  summarise(n_keyword = sum(n_keyword),
            rank = head(rank, 1)
  ) %>% 
  ungroup()


df.long_join <- df.long %>% 
  left_join(df.wordlen, by = c("rank", "name")) %>%
  mutate(per_keyword = n_keyword/n)

# Plot
df.plot <- df.long_join %>%
  mutate(sdg_number = as.numeric(str_extract(sdg, "\\d+"))) %>%
  mutate(sdg = as_factor(sdg)) %>%
  mutate(sdg = fct_reorder(sdg, sdg_number)) %>% 
  filter(!is.na(sdg))

