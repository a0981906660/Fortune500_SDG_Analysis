#' @title 06_2_most_frequent_keyword_mining_vs_allfirms.R
#' @description
#'   Three top-20 keyword-frequency tables, same logic as 06_1:
#'     Table 1 - MINING       : NAICS 21 firms only (corrected wordcount)
#'     Table 2 - ALL FIRMS    : every firm across all 19 NAICS wordcount files
#'                              (NAICS 21 uses the corrected file)
#'     Table 3 - NON-MINING   : all firms EXCLUDING NAICS 21
#'                              (the 18 non-21 industry wordcount files)
#'
#'   "Mining" = NAICS 21 (df_wordCount_NAICS21_correct.RDS). Removing mining
#'   therefore means simply not reading the NAICS 21 file. Other industries'
#'   collapse (if any) is left as-is and does not affect a cross-firm
#'   cross-year frequency ranking.
#'
#'   06_1 is NOT modified. Only config.R / func.R are sourced.

rm(list = ls())

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(kableExtra)
source("./code/utils/func.R")
source("./code/config.R", encoding = '')

df_final_key <- read_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.RDS")
)
df.Gvkey <- getGvkeyMap(
  glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx")
)

NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52,
                  53, 54, 60, 62, 72)

TOP_N <- 20   # <- number of keywords per table (was 15)

# NAICS 21 -> corrected file; everything else -> original file
wordcount_path <- function(naics) {
  if (naics == 21) {
    glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS21_correct.RDS")
  } else {
    glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS{naics}.RDS")
  }
}

# 06_1's findMostFreqKeyword inner logic, taking a data frame.
# Candidate pool widened to 60 before the NA-sdg filter so that trimming to
# TOP_N afterwards still has enough valid rows (avoids NA padding).
topFreqKeyword <- function(df.wordCount) {
  df.wordCount %>%
    mutate(
      year = str_extract(name, "\\d{4}$"),
      name = str_remove(name, "_\\d{4}$")
    ) %>%
    left_join(df.Gvkey, by = "name") %>%
    filter(n_keyword > 0) %>%
    left_join(df_final_key, by = c("keyword" = "word")) %>%
    distinct() %>%
    arrange(desc(n_keyword)) %>%
    group_by(original_keyword, sdg) %>%
    summarise(original_keyword = head(original_keyword, 1),
              keyword          = head(keyword, 1),
              n_keyword        = sum(n_keyword),
              sdg              = list(unique(sdg)),
              .groups = "drop") %>%
    arrange(desc(n_keyword)) %>%
    distinct() %>%
    slice(1:60) %>%
    select(sdg, original_keyword, n_keyword) %>%
    mutate(sdg = unlist(sdg)) %>%
    select(original_keyword, n_keyword, sdg) %>%
    mutate(original_keyword = str_remove_all(original_keyword, "[[:punct:]]")) %>%
    filter(!is.na(sdg))
}

# Pool a set of NAICS codes into one wordcount frame
load_pool <- function(codes) {
  out <- tibble()
  for (naics in codes) {
    cat(">>> loading NAICS", naics, "\n")
    out <- bind_rows(out, read_rds(wordcount_path(naics)))
  }
  out
}

# ---------------------------------------------------------------------------
# Table 1: MINING (NAICS 21, corrected file)
# ---------------------------------------------------------------------------
cat(">>> Table 1: Mining (NAICS 21)\n")
tab_mining <- topFreqKeyword(read_rds(wordcount_path(21)))

# ---------------------------------------------------------------------------
# Table 2: ALL FIRMS (all 19 industries)
# ---------------------------------------------------------------------------
cat(">>> Table 2: All firms (19 industries)\n")
tab_all <- topFreqKeyword(load_pool(NAICS2_CODES))

# ---------------------------------------------------------------------------
# Table 3: NON-MINING (all industries EXCEPT NAICS 21)
# ---------------------------------------------------------------------------
cat(">>> Table 3: Non-mining (exclude NAICS 21)\n")
tab_nonmining <- topFreqKeyword(load_pool(setdiff(NAICS2_CODES, 21)))

# ---------------------------------------------------------------------------
# Trim each to TOP_N and assemble
# ---------------------------------------------------------------------------
tab_mining    <- tab_mining[1:TOP_N, ]
tab_all       <- tab_all[1:TOP_N, ]
tab_nonmining <- tab_nonmining[1:TOP_N, ]

mk <- function(tab) {
  bind_cols(
    Keyword     = tab$original_keyword,
    `SDG`       = tab$sdg,
    `Frequency` = tab$n_keyword
  )
}
df.table_mining    <- mk(tab_mining)
df.table_all       <- mk(tab_all)
df.table_nonmining <- mk(tab_nonmining)

# ---------------------------------------------------------------------------
# Preview
# ---------------------------------------------------------------------------
df.table_mining %>%
  kbl(caption = glue("Top {TOP_N} frequent keywords - Mining (NAICS 21)")) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

df.table_all %>%
  kbl(caption = glue("Top {TOP_N} frequent keywords - All firms")) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

df.table_nonmining %>%
  kbl(caption = glue("Top {TOP_N} frequent keywords - Non-mining firms")) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

# ---------------------------------------------------------------------------
# Export LaTeX (three separate files)
# ---------------------------------------------------------------------------
df.table_mining %>%
  kbl(caption = glue("Top {TOP_N} frequent keywords (Mining, NAICS 21)"),
      booktabs = TRUE, format = "latex") %>%
  save_kable(file = "./data/result/tab_keyword_frequency_mining.tex")

df.table_all %>%
  kbl(caption = glue("Top {TOP_N} frequent keywords (All firms)"),
      booktabs = TRUE, format = "latex") %>%
  save_kable(file = "./data/result/tab_keyword_frequency_allfirms.tex")

df.table_nonmining %>%
  kbl(caption = glue("Top {TOP_N} frequent keywords (Non-mining firms)"),
      booktabs = TRUE, format = "latex") %>%
  save_kable(file = "./data/result/tab_keyword_frequency_nonmining.tex")

cat("\n>>> Done. Three tables saved:\n")
cat("    ./data/result/tab_keyword_frequency_mining.tex\n")
cat("    ./data/result/tab_keyword_frequency_allfirms.tex\n")
cat("    ./data/result/tab_keyword_frequency_nonmining.tex\n")