# rm(list = ls())
rm(list = setdiff(ls(), c("NAICS2_CODE", "NAICS2_CODES", "timeSpent", "t0", "t1")))
library(tidyverse)
library(tidytext)
library(stringi)
library(fs)
library(foreach)
library(doParallel)
library(furrr)
source("./code/utils/func.R", encoding = '')

# define function used in parallel computing
countWords <- function(splited_data) {
  counter <- 0
  counter_end <- length(df_final_key$word)
  df_keyword_n <- tibble()
  for (keyword in df_final_key$word) {
    name <- splited_data$name[1] 
    counter <- counter + 1
    cat(">>> Progress: ", round(counter/counter_end, 4)*100, "%", "\n>>> Company : ", name, "\n") 
    n_keyword <- splited_data %>% 
      mutate(text = tolower(text)) %>% 
      pull(text) %>% 
      map_dbl(~stringi::stri_count(., regex = keyword)) %>% 
      sum(.)
    df_keyword_n_tmp <- tibble(name = name,
                               n_keyword,
                               keyword)
    df_keyword_n <- df_keyword_n %>% bind_rows(df_keyword_n_tmp)
  }
  return(df_keyword_n)
}

# Registrate workers
n_cores <- 10
cl <- makeCluster(n_cores)
# clusterEvalQ(cl, library(matlib))
clusterEvalQ(cl, library(purrr))
clusterEvalQ(cl, library(dplyr))
registerDoParallel(cl)
# registerDoParallel(n_cores) # use multicore, set to the number of cores

# Load data
df_final_key <- read_rds("./data/cleaned_data/df_final_key.rds")

# IO of annual reports
df.RankCode <- getRankCodeMap("./data/raw_data/TM Final_FortuneG500 (2021)_v2.xlsx")

## A BETTER WAY TO SELECT NAICS STARTING WITH XX
## Change the following code if needed
df.doc <- readReports(NAICS2_CODE)


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

# # Parallel computing
# ## testing
# set.seed(12345)
# df_sentence_sample <- df_sentence[sample(1:nrow(df_sentence), 10),]
# dfs_sentence <- split(df_sentence_sample, df_sentence_sample$name)
# length(dfs_sentence)
# # do parallel
# t0 <- Sys.time()
# res <- foreach (x = dfs_sentence,
#                 .combine = rbind) %dopar% {
#                   countWords(x)
#                 }
# t1 <- Sys.time()
# cat(">>> Time used: ", format(t1 - t0), "\n")


## Applying
## split dataframe by firm-year value
dfs_sentence <- split(df_sentence, df_sentence$name)
length(dfs_sentence)

# do parallel
t1 <- Sys.time()
cat(">>> Doing parallel computing with ", n_cores, " cores.\n")
res <- foreach (x = dfs_sentence,
                .combine = rbind) %dopar% {
                  countWords(x)
                }
t2 <- Sys.time()
cat(">>> Time used: ", format(t2 - t1), "\n")

## Save files
path_name <- paste0("./data/cleaned_data/df_wordCount_NAICS", NAICS2_CODE, ".rds")
res %>% write_rds(path_name)

parallel::stopCluster(cl)






