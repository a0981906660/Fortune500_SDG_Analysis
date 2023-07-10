#' Rerun selected word counts

rm(list = ls())

NAICS2_CODES <- c(21, 31, 33)
# NAICS2_CODES <- c(33)

timeSpent <- c()
for (NAICS2_CODE in NAICS2_CODES) {
  # timing
  t0 <- Sys.time()
  source("./code/02_1_parallel_wordCount.R")
  t_end <- Sys.time()
  timeSpent <- c(timeSpent, t_end - t0)
  cat(">>> Time used: ", format(t_end - t0), "\n")
}

