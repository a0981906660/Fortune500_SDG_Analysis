rm(list = ls()) #把環境清空
# Load necessary packages
library(tidyverse)
library(readxl)
library(hash)

# Setup working directory
getwd()

#' @function
# keyword_file <- "./data/raw_data/keyword/Keyword_SDSN.csv"

process_keywords <- function(keyword_file) {
    
  # Load keyword dictionary
  df_keySDSN <- read_csv(keyword_file) %>% 
    rename(word = `SDG Keywords`, word2 = `Alternatives`, sdg = `SDG`)
  
  # Combine the 2 colums -> use keywords in both columns at once
  df_keyunnest_SDSN <- df_keySDSN %>% 
    select(sdg, word) %>% 
    bind_rows(df_keySDSN %>% 
                select(word2, sdg) %>% 
                drop_na() %>% 
                rename(word = word2) #需要了解rename function
    ) %>% 
    # sort the rows in SDG order
    mutate(SDG_order = str_extract(sdg, pattern = "\\d+"),
           SDG_order = as.numeric(SDG_order),
    ) %>% 
    arrange(SDG_order) %>% 
    select(-SDG_order) %>%
    # trim white spaces at both sides
    mutate(word = str_trim(word, side = "both")) %>% 
    # add regular expressions
    mutate(word = str_replace_all(word, "\\*", ".?")) %>%
    mutate(word = str_replace_all(word, " AND ", ".*?")) %>% 
    #舉例 Economic Resource AND Access 在一個句子裡面同時出現 可在df_key_final 看
    mutate(word = str_split(word, "; ")) %>% #把excel 裡面同一格有 分號; 的分開到不同row 如row 101，不一定要前後
    unnest(c(word)) %>% 
    distinct() %>% 
    mutate(ID = row_number())
  
  # Create a dataframe of keywords without spaces -> nspace
  df_keynspace_SDSN <- df_keyunnest_SDSN %>%
   filter(!str_detect(word, " ")) %>%
   mutate(lll = str_length(word),
          lll_b = str_count(word, "[A-Z](s?)"),
          ends_with_s = str_detect(word, "s$"),
          minus = lll-lll_b,) %>%
          mutate(word = if_else(lll > lll_b & !minus=="1", str_to_lower(word), word)) %>%
   # convert all the keywords into lower-case
   mutate(word = tolower(word))
  
  
  # Create a dataframe of keywords with spaces
  df_keyspace_SDSN <- df_keyunnest_SDSN %>% 
    filter(str_detect(word," ")) %>%
    mutate(word = str_to_lower(word))
  
  # Create a dataframe to merge the dataframe with and without spaces
  df_bind_SDSN <- df_keynspace_SDSN %>% 
    bind_rows(df_keyspace_SDSN) %>%
    mutate(word = str_trim(word, "both")) %>%
    mutate(word = if_else(str_detect(word, "\\.\\*\\?"), 
                          word, str_c("\\b",word,"\\b"))) %>%
    # sort the rows in SDG order
    mutate(SDG_order = str_extract(sdg, pattern = "\\d+"),
           SDG_order = as.numeric(SDG_order),
    ) %>% 
    arrange(SDG_order) %>% 
    select(ID, sdg, word) 
  
  # clean up env 
  # rm= remove setdiff=setdifference ls()= environment裡面全部東西列出 c(內容)=要留的
   rm(list=setdiff(ls(), c("df_bind_SDSN", "df_keyunnest_SDSN")))
   
  # clean up
  df_final_key_SDSN <- df_bind_SDSN %>%
    mutate(word = str_replace_all(word, "\\{", "(")) %>%
    mutate(word = str_replace_all(word, "\\}", ")")) %>%
    mutate(word = str_remove_all(word, "“|”")) %>%
    filter(!str_detect(word, "not")) %>%
    distinct() %>%
    filter(!str_detect(word,"goverance")) #看看要不要governance 這個字
  
  # merge back the original keyword
  df_final_key_SDSN <- df_final_key_SDSN %>% 
    left_join(df_keyunnest_SDSN %>% 
                select(ID, original_keyword = word), 
              by = "ID") %>% 
    arrange(ID)
  
  # make sure the keywords are distinct
  df_final_key_SDSN <- df_final_key_SDSN %>% 
    distinct()
  
  return(df_final_key_SDSN)
}

df_all <- process_keywords("./data/raw_data/keyword/Keyword_all.csv")
df_SDSN <- process_keywords("./data/raw_data/keyword/Keyword_SDSN.csv")

df_all %>% write_rds("./data/cleaned_data/df_final_key_all.rds")
df_SDSN %>% write_rds("./data/cleaned_data/df_final_key_SDSN.rds")

