#' Rerun selected word counts

rm(list = ls())

# NAICS2_CODES <- c(21, 31, 33)
NAICS2_CODES <- c(33)

timeSpent <- c()
for (NAICS2_CODE in NAICS2_CODES) {
  # timing
  t0 <- Sys.time()
  source("./code/02_1_countH_adjusted.R")
  t1 <- Sys.time()
  timeSpent <- c(timeSpent, t1 - t0)
  cat(">>> Time used: ", format(t1 - t0), "\n")
}
