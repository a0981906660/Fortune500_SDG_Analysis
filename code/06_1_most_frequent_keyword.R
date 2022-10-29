rm(list = ls())
#' 06
#' @description This script creates a table that lists the most frequent keywords across industies
#' NAICS code starting with 21, 31, 33 are considered for now
#' the common most frequent keywords may need further adjustment

library(dplyr)
library(readr)
library(stringr)
library(kableExtra)

findMostFreqKeyword <- function(NAICS_Code2) {
  # Load data based on NAICS code
  df.wordCount <- read_rds(paste0("./data/cleaned_data/df_wordCount_NAICS", NAICS_Code2, ".rds"))
  df_final_key <- read_rds("./data/cleaned_data/df_final_key.rds")
  
  df.wordCount %>% filter(n_keyword > 0) %>% 
    # merge back the original keyword
    left_join(df_final_key, by = c("keyword" = "word")) %>% 
    # there are duplicates coz a keyword may be in multiple SDG categories
    select(-sdg) %>% 
    distinct() %>% 
    # sort by frequency
    arrange(desc(n_keyword)) %>% 
    group_by(original_keyword) %>% 
    summarise(original_keyword = head(original_keyword, 1),
              keyword = head(keyword, 1),
              n_keyword = max(n_keyword)) %>% 
    arrange(desc(n_keyword)) %>% 
    distinct() %>% 
    # take the top 15 most frequent
    slice(1:15) %>% 
    pull(original_keyword) %>% 
    return()
}

# find the most frequent keywords across industry
col21 <- findMostFreqKeyword(21)
col31 <- findMostFreqKeyword(31)

df.table <- bind_cols(
  NAICS21 = col21,
  NAICS31 = col31
)

# make a table
kbl(df.table) %>% 
  kable_paper("hover", full_width = F)

df.table %>% 
  kbl(caption = "Recreating booktabs style table") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# export latex code
df.table %>% 
  kbl(caption = "test", booktabs = T, format = 'latex') %>% 
  kable_styling(latex_options = c("striped", "hold_position")) %>% 
  save_kable(file = './data/result/tab_keyword_frequency.tex')
