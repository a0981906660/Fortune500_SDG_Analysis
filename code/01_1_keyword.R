rm(list = ls()) #把環境清空
# Load necessary packages
library(tidyverse)
library(readxl)
library(hash)

# Setup working directory
setwd("F:/Fortune500_SDG_Analysis")
getwd()

# Load keyword dictionary
df_keyword <- readxl::read_excel("./data/raw_data/UoA-SDG-Keyword-List-Ver.-1.1.xlsx", 
                                 sheet = "keyword") %>% 
  rename(word = `SDG Keywords`, word2 = `Alternatives`, sdg = `SDG`)

# Combine the 2 colums -> use keywords in both columns at once
df_keyword_unnest <- df_keyword %>% 
  select(sdg, word) %>% 
  bind_rows(df_keyword %>% 
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
df_keyword_nspace <- df_keyword_unnest %>% 
  filter(!str_detect(word, " ")) %>% 
  mutate(lll = str_length(word), 
         lll_b = str_count(word, "[A-Z]")) %>%
  mutate(word = if_else(lll > lll_b, str_to_lower(word), word))

#-----------我感覺以下這樣可以更Refine 輸入進來的keyword, 特別是縮寫---------------
#df_keyword_nspace <- df_keyword_unnest %>% 
#  filter(!str_detect(word, " ")) %>% 
#  mutate(lll = str_length(word), 
#         lll_b = str_count(word, "[A-Z]"),
#         ends_with_s = str_detect(word, "s$"),
#         minus = lll-lll_b,) %>%
#         mutate(word = if_else(lll > lll_b, str_to_lower(word), word))
#         mutate(word = if_else(lll > lll_b & !minus=="1", str_to_lower(word), word))
#--------------------------------------------------------------------------------

# Create a dataframe of keywords with spaces
df_keyword_space <- df_keyword_unnest %>% 
  filter(str_detect(word," ")) %>%
  mutate(word = str_to_lower(word))

# Create a dataframe to merge the dataframe with and without spaces
df_bind<- df_keyword_nspace %>% 
  bind_rows(df_keyword_space) %>%
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
rm(list=setdiff(ls(), c("df_bind", "df_keyword_unnest")))

# Load the manual edited keyword mapping
# https://docs.google.com/spreadsheets/d/1fZdE9WcFYI_d_sD4BgBpI5D1QhOYlRngsuSEtB7w694/edit#gid=470532546

df_manual <- read_excel("./data/raw_data/manual_edit_keywords.xlsx",
                        sheet = "df_manual")
h <- hash(keys = df_manual$word, values = df_manual$word_new)
# e.g. 
# h[[df_manual$word[1]]]
mapKeyVal <- function(keys) {
  res <- c()
  for (key in keys) {
    res <- c(res, h[[key]])
  }
  return(res)
}
# df_manual$word[1:3] %>% mapKeyVal()

# replace the original keywords
df_bind <- df_bind %>% 
  filter(!str_detect(word, " or ")) %>% 
  # replace "or" as | 
  bind_rows(
    df_bind %>% 
      filter(str_detect(word, " or ")) %>% 
      mutate(word = str_replace_all(word, " or ", "|"))
  ) %>% 
  mutate(word = ifelse(word %in% df_manual$word,
                        mapKeyVal(word),
                        word))
# clean up
df_final_key <- df_bind %>%
  mutate(word = str_replace_all(word, "\\{", "(")) %>%
  mutate(word = str_replace_all(word, "\\}", ")")) %>%
  mutate(word = str_remove_all(word, "“|”")) %>%
  filter(!str_detect(word, "not")) %>%
  distinct() %>%
  filter(!str_detect(word,"goverance")) #看看要不要governance 這個字

# merge back the original keyword
df_final_key <- df_final_key %>% 
  left_join(df_keyword_unnest %>% 
              select(ID, original_keyword = word), 
            by = "ID") %>% 
  arrange(ID)

# make sure the keywords are distinct
df_final_key <- df_final_key %>% 
  distinct()

# Save file
df_final_key %>% 
  write_rds("./data/cleaned_data/df_final_key.rds")



#----------------用SDSN Keyword list 且不加入manual 檔------------------------------------

# Load keyword dictionary
df_keySDSN <- readxl::read_excel("./data/raw_data/UoA-SDG-Keyword-List-Ver.-1.1.xlsx", 
                                 sheet = "SDSN") %>% 
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
  mutate(lll = str_length(word), lll_b = str_count(word, "[A-Z]")) %>%
  mutate(word = if_else(lll > lll_b, str_to_lower(word), word))

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

# Load the manual edited keyword mapping
# https://docs.google.com/spreadsheets/d/1fZdE9WcFYI_d_sD4BgBpI5D1QhOYlRngsuSEtB7w694/edit#gid=470532546

# df_manual <- read_excel("./data/raw_data/manual_edit_keywords.xlsx",
#                       sheet = "df_manual")
# h <- hash(keys = df_manual$word, values = df_manual$word_new)
# e.g. 
# h[[df_manual$word[1]]]
# mapKeyVal <- function(keys) {
#  res <- c()
#  for (key in keys) {
#    res <- c(res, h[[key]])
#  }
#  return(res)
# }
# df_manual$word[1:3] %>% mapKeyVal()

# replace the original keywords
# df_bind <- df_bind %>% 
#  filter(!str_detect(word, " or ")) %>% 
  # replace "or" as | 
#  bind_rows(
#    df_bind %>% 
#      filter(str_detect(word, " or ")) %>% 
#      mutate(word = str_replace_all(word, " or ", "|"))
#  ) %>% 
#  mutate(word = ifelse(word %in% df_manual$word,
#                       mapKeyVal(word),
#                       word))
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

# Save file
df_final_key_SDSN %>% 
  write_rds("./data/cleaned_data/df_final_key_SDSN.rds")

