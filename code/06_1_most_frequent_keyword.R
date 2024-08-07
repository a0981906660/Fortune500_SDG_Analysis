rm(list = ls())

#' 06
#' @description This script creates a table that lists the most frequent keywords across industies
#' NAICS code starting with 21, 31, 33 are considered for now
#' the common most frequent keywords may need further adjustment

library(dplyr)
library(readr)
library(stringr)
library(kableExtra)
source("./code/config.R", encoding = '')

findMostFreqKeyword <- function(NAICS_Code2) {
  # Load data based on NAICS code 
  # 可以在console 打NAICS_Code2 <- 21 (或其他產業)後看df.wordCount
  df.wordCount <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS{NAICS_Code2}.RDS"))
  # df_final_key <- read_rds("./data/cleaned_data/df_final_key.rds")
  # df_final_key <- read_rds("./data/cleaned_data/df_final_key_all.rds")
  df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.RDS"))
  
  df.res <- df.wordCount %>% filter(n_keyword > 0) %>% 
    # merge back the original keyword
    left_join(df_final_key, by = c("keyword" = "word")) %>% 
    distinct() %>% 
    # sort by frequency
    arrange(desc(n_keyword)) %>% 
    group_by(original_keyword, sdg) %>% 
    summarise(original_keyword = head(original_keyword, 1),
              keyword = head(keyword, 1),
              n_keyword = sum(n_keyword), #這裡可以改max or mean
              sdg = list(unique(sdg))) %>% 
    ungroup() %>% 
    arrange(desc(n_keyword)) %>% 
    distinct() %>% 
    # take the top 15 most frequent
    slice(1:20) %>% 
    #add or remove n_keyword to include or exclude frequency column
    select(sdg, original_keyword, n_keyword) %>% #add or remove n_keyword here
    mutate(sdg = unlist(sdg)) %>% 
    # reorder columns
    select(original_keyword, n_keyword, sdg) %>% #add or remove n_keyword here
    # Remove special characters
    mutate(original_keyword = stringr::str_remove_all(original_keyword, "[[:punct:]]")) %>% 
    filter(!is.na(sdg))
    
    return(df.res)
}

# find the most frequent keywords across industry
MAX_ENTRIES <- 15
col21 <- findMostFreqKeyword(21)
col31 <- findMostFreqKeyword(31)
col33 <- findMostFreqKeyword(33)

col21 <- col21[1:MAX_ENTRIES, ]
col31 <- col31[1:MAX_ENTRIES, ]
col33 <- col33[1:MAX_ENTRIES, ]

# TODO: delete all 3 `Frequency` to drop out frequency column
df.table <- bind_cols(
  NAICS21 = col21$original_keyword,
  `SDG` = col21$sdg,
  `Frequency` = col21$n_keyword,
  # TODO: if only want column 21, comment codes below
  NAICS31 = col31$original_keyword,
  `SDG ` = col31$sdg,
  `Frequency ` = col31$n_keyword,
  NAICS33 = col33$original_keyword,
  `SDG  ` = col33$sdg,
  `Frequency  ` = col33$n_keyword
 )


# make a table
kbl(df.table) %>% 
  kable_paper("hover", full_width = F)

df.table %>% 
  kbl(caption = "Top 15 frequent keywords show up in the documents") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# export latex code
df.table %>% 
  kbl(caption = "test", booktabs = T, format = 'latex') %>% 
  # kable_styling(latex_options = c("striped", "hold_position")) %>% 
  save_kable(file = './data/result/tab_keyword_frequency.tex')

