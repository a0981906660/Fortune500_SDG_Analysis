#' Rerun selected word counts

rm(list = ls())

NAICS2_CODES <- c(21, 31, 33)
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