# rm(list = ls())
rm(list = setdiff(ls(), c("NAICS2_CODE", "NAICS2_CODES", "timeSpent", "t0", "t1")))
# NAICS2_CODE = 21
library(tidyverse)
library(readr)
library(tidytext)
library(stringi)
library(fs)
library(foreach)
library(doParallel)
library(furrr)
library(doFuture)
library(progressr)
source("./code/utils/func.R", encoding = '')

# Set up parallel backend
# Registrate workers
n_cores <- parallel::detectCores() - 1  # Leave one core free for system processes
cl <- makeCluster(n_cores)
clusterEvalQ(cl, library(purrr))
clusterEvalQ(cl, library(dplyr))
registerDoParallel(cl)


# define function used in parallel computing
# index, total, and NAICS codes are passed only for logging
countWords <- function(splited_data, index = NULL, total = NULL, the_naics_code = NULL) {

  # loggin into file
  progress_message <- sprintf("Processing %d of %d\n", index, total)
  write_lines(progress_message, paste0("./logs/progress_", the_naics_code, , "_", index, ".log"))
  t0_sub <- Sys.time()
  
  counter <- 0
  counter_end <- length(df_final_key$word)
  df_keyword_n <- tibble()
  for (keyword in df_final_key$word) {
    
    # logging
    name <- splited_data$name[1] 
    counter <- counter + 1
    
    if (counter %% 100 == 0) {
      t1_sub <- Sys.time()
      progress_message <- paste0(">>> Progress: ", round(counter/counter_end, 4)*100, "%", "\n>>> Company : ", name, "\n",
                                 "Time spending:", format(t1_sub - t0_sub), "\n",
                                 "Avg time spend:", format((t1_sub - t0_sub)/counter) ) 
      write_lines(progress_message, paste0("./logs/progress", index, ".log"), append = TRUE)
    }
    
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


# Load data
df_final_key <- read_rds("./data/cleaned_data/df_final_key_all.rds")

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

# Parallel computing
## split dataframe by firm-year value
dfs_sentence <- split(df_sentence, df_sentence$name)
length(dfs_sentence)

# Create a progress bar
# total <- length(dfs_sentence)
# pb <- progress::progress_bar$new(format = "  [:bar] :percent :elapsed", total = total, clear = FALSE, width = 60)

# do parallel
t1 <- Sys.time()
cat(">>> Doing parallel computing with ", n_cores, " cores.\n")
# Modify the foreach loop to update the progress bar
res <- foreach(x = seq_along(dfs_sentence), 
               .combine = rbind, 
               # .export = c("pb"), 
               .packages = c("dplyr", "purrr", "stringi", "progress", "readr")) %dopar% {
  result <- countWords(dfs_sentence[[x]], 
                       index = x, 
                       total = length(dfs_sentence), 
                       the_naics_code = NAICS2_CODE)
  # pb$tick()  # Update the progress bar
  # result
}
t2 <- Sys.time()
cat(">>> Time used: ", format(t2 - t1), "\n")


## Save files
path_name <- paste0("./data/cleaned_data/df_wordCount_NAICS", NAICS2_CODE, ".rds")
res %>% write_rds(path_name)

parallel::stopCluster(cl)

