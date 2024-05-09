#' Rerun selected word counts

rm(list = ls())

NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
# NAICS2_CODES <- c(21) 如果只要（重）跑特定一個產業，即單獨放那個產業
# all industries in Excel: TM Final_FortuneG500 (2021)_v2
# NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
# 19 industries in total
# 55 沒有年報 是空的

timeSpent <- c()
for (NAICS2_CODE in NAICS2_CODES) {
  # timing
  t0 <- Sys.time()
  # source("./code/02_1_parallel_wordCount.R")
  source("./code/02_1_parallel_wordCount_progressbar.R")
  t_end <- Sys.time()
  timeSpent <- c(timeSpent, t_end - t0)
  cat(">>> Time used: ", NAICS2_CODE, ": ",format(t_end - t0), "\n")
  # NAICS2 21; Time used:  21.80071 hours
  # NAICS2 31; Time used:  6.595993 hours
  # NAICS2 33; Time used:  6.782853 hours
}
#>>> Time used:  21 :  9.268672 hours 
#>>> Time used:  31 :  2.540388 hours 
#>>> Time used:  33 :  1.053115 hour

# 7 May 2024 time note:
# 72 :  1.649501 mins
# 62 :  5.362494 mins
# 60 :  1.818162 mins
# 54 :  5.521456 mins
# 53 :  1.421968 mins
# 52 :  1.847637 hours
# 51 :  20.01726 mins
# 49 :  4.456856 mins
# 48 :  7.56963 mins
# 45 :  10.33758 mins
# 44 :  15.69932 mins
# 42 :  18.2359 mins 
# 33 :  1.097597 hours
# 32 :  39.86953 mins
# 31 :  2.603275 hours
# 23 :  9.293315 mins
# 22 :  12.19108 mins
# 21 :  9.315039 hours
# 11 :  1.165202 mins











