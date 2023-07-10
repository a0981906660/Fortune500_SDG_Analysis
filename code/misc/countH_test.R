rm(list = ls())
rm(list = setdiff(ls(), c("NAICS2_CODE", "NAICS2_CODES", "timeSpent", "t0", "t1")))
library(tidyverse)
library(tidytext)
library(stringi)
library(fs)

source("./code/utils/func.R", encoding = '')

# Load data
# df_final_key <- read_rds("./data/cleaned_data/df_final_key.rds")
df_final_key <- read_rds("./data/cleaned_data/df_final_key_SDSN.rds")

# IO of annual reports
df.RankCode <- getRankCodeMap("./data/raw_data/TM Final_FortuneG500 (2021)_v2.xlsx")

## TODO: change the content of the following test file
test_filepath <- "/Volumes/buffalo/4_teaching/Fortune500_SDG_Analysis/data/raw_data/1 Walmart_2015.txt"

# read a specific text file
# overwrite the existing function with the same name
readReports <- function(filepath) {
  
  ## Create a new df to store the path of the file to read and the contents
  df_doc <- tibble()
  
    rank <- str_extract(filepath, "\\d\\s")
    year <- str_extract(filepath, "\\d{4}")
    name <- str_extract(filepath, "\\w")
    df.tmp <- read_lines(filepath) %>% 
      as_tibble() %>% 
      summarise(value = str_c(value, collapse = "\\s")) %>% 
      mutate(name = name,
             rank = rank,
             rank = as.numeric(rank),
             year = as.numeric(year),
             name = paste0(name, "_", year)) %>% 
      drop_na() %>% 
      arrange(rank, year)
    df_doc <- df_doc %>% bind_rows(df.tmp)
  
  return(df_doc)
}

df.doc <- readReports(test_filepath)


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
path_name <- paste0("./data/cleaned_data/df_wordCount_playground.rds")
df_keyword_n %>% write_rds(path_name)

