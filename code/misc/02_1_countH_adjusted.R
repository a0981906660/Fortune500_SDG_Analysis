rm(list = ls())
rm(list = setdiff(ls(), c("NAICS2_CODE", "NAICS2_CODES", "timeSpent", "t0", "t1")))
library(tidyverse)
library(tidytext)
library(stringi)
library(fs)

source("./code/utils/func.R", encoding = '')

# Load data
df_final_key <- read_rds("./data/cleaned_data/df_final_key.rds")
df_final_key_old <- read_rds("./data/rds/df_final_key.rds")

# a comparison: check correctness
setdiff(df_final_key$word, df_final_key_old$word) # A集合 差集 B集合，新的比舊的多些什麼
setdiff(df_final_key_old$word, df_final_key$word) # B - A, 舊的比新的多什麼 -> 沒有

# IO of annual reports
df.RankCode <- getRankCodeMap("./data/raw_data/TM Final_FortuneG500 (2021)_v2.xlsx")

## A BETTER WAY TO SELECT NAICS STARTING WITH XX
## TODO: Change the following code if needed
# NAICS2_CODE <- 31
while(!exists("NAICS2_CODE")) {
  NAICS2_CODE <- readline("Please enter NAICS2 Code; can be either 31 or 33:  ")
  NAICS2_CODE <- as.numeric(NAICS2_CODE)
  df.doc <- readReports(NAICS2_CODE)
}


# compute word count
## post-processing the read files
#' @note "\x0b" is a special non-UTF8 character that cannot be encoded, so we need to remove it
#' https://stackoverflow.com/questions/43533486/utf8-null-character-normalizing-whitespace-characters
df_sentence <- df.doc %>% 
  # convert character vector between encodings
  mutate(value = stringr::str_replace(value, "\x0b", "")) %>% 
  # mutate(value = iconv(value, "", "utf8", sub = "")) %>%
  unnest_tokens(output = text, input = value, token = "sentences") %>% 
  drop_na() %>% 
  arrange(rank)


## New an empty tibble to store the final result
df_keyword_n <- tibble()
total_progress <- length(unique(df_sentence$name))*length(df_final_key$word)
counter <- 0
for (txtfile in unique(df_sentence$name)) {
  for (keyword in df_final_key$word) {
    counter <- counter + 1
    message(">>> Progress: ", round(counter/total_progress, digits = 4)*100, "%")
    
    n_keyword <- df_sentence %>% 
      filter(name == txtfile) %>% 
      mutate(text = tolower(text)) %>% 
      pull(text) %>% 
      map_dbl(~stringi::stri_count(., regex = keyword)) %>% 
      sum(.)
    df_keyword_n_tmp <- tibble(name = txtfile,
                               n_keyword,
                               keyword)
    df_keyword_n <- df_keyword_n %>% bind_rows(df_keyword_n_tmp)
  }
}

## Save files
path_name <- paste0("./data/cleaned_data/df_wordCount_NAICS", NAICS2_CODE, ".rds")
df_keyword_n %>% write_rds(path_name)

