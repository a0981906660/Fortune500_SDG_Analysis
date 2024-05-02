#' Rerun selected word counts

rm(list = ls())

NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
# all industries in Excel: TM Final_FortuneG500 (2021)_v2
# NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
# 55 沒有年報 是空的

# NAICS2_CODES <- c(21)

timeSpent <- c()
for (NAICS2_CODE in NAICS2_CODES) {
  # timing
  t0 <- Sys.time()
  source("./code/02_1_parallel_wordCount.R")
  t_end <- Sys.time()
  timeSpent <- c(timeSpent, t_end - t0)
  cat(">>> Time used: ", NAICS2_CODE, ": ",format(t_end - t0), "\n")
  # NAICS2 21; Time used:  21.80071 hours
  # NAICS2 31; Time used:  6.595993 hours
  # NAICS2 33; Time used:  6.782853 hours
}
#>>> Time used:  21 :  9.268672 hours 
#>>> Time used:  31 :  2.540388 hours 
#>>> Time used:  33 :  1.053115 hours 