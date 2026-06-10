#' @title 02_5_fix_eight_firms_wordcount.R  (STANDALONE - edits no other script)
#' @description
#'   Eight NAICS-21 firms had `year` mis-parsed from the gvkey in the folder
#'   path (their gvkey contains a 20xx pattern), so every annual report of each
#'   firm collapsed into ONE fake-year row in df_wordCount_NAICS21.RDS
#'   (e.g. Vale_2093 = the sum over all of Vale's years).
#'
#'   This script re-reads ONLY those eight folders, parses the year correctly
#'   from the FILE NAME, recomputes per-firm-year word counts with the SAME
#'   logic as count_keyword_frequency_parallel.R, then:
#'     1. loads existing df_wordCount_NAICS21.RDS              (left on disk untouched)
#'     2. drops the eight fake-year rows (matched by exact name)
#'     3. binds the freshly recomputed correct rows
#'     4. writes to df_wordCount_NAICS21_correct.RDS           (NEW file)
#'
#'   The original df_wordCount_NAICS21.RDS is NEVER overwritten.
#'   Only config.R and func.R are sourced; neither is modified.

rm(list = ls()); gc()

library(tidyverse)
library(readr)
library(tidytext)
library(stringi)
library(fs)
library(glue)

source("./code/config.R", encoding = "")        # for DROPBOX_PATH
source("./code/utils/func.R", encoding = "")     # sourced only, not modified

# ---------------------------------------------------------------------------
# keyword dictionary (identical source as the parallel counter)
# ---------------------------------------------------------------------------
df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.RDS"))

REPORT_ROOT <- glue("{DROPBOX_PATH}/raw_data/Fortune_500_report")

# ---------------------------------------------------------------------------
# Read one firm folder -> tidy (value, name, gvkey, year) at firm-YEAR level.
#   - only .txt files (verified: folders contain pdf + txt, no htm)
#   - year = LAST 20xx in basename(file); gvkey lives in the parent dir so it
#     can never be picked up here
#   - name = "{display}_{year}", display = folder name minus "{6 digits}_"/"_"
#     (identical construction to readReports())
# ---------------------------------------------------------------------------
read_one_firm <- function(folder_name) {
  
  folder_path  <- glue("{REPORT_ROOT}/{folder_name}")
  display_name <- str_replace(folder_name, "^(\\d{6}_|_)", "")
  gvkey        <- str_extract(folder_name, "^\\d{6}")
  
  txt_files <- fs::dir_ls(folder_path, recurse = TRUE, regexp = "\\.txt$")
  
  df_firm <- tibble()
  for (txt in txt_files) {
    yr_all <- stringr::str_extract_all(basename(txt), "20\\d{2}")[[1]]
    year   <- if (length(yr_all) == 0) NA_character_ else tail(yr_all, 1)
    
    df_tmp <- read_lines(txt) %>%
      as_tibble() %>%
      summarise(value = str_c(value, collapse = "\\s")) %>%
      mutate(
        name  = display_name,
        gvkey = gvkey,
        year  = as.numeric(year),
        name  = paste0(name, "_", year)
      ) %>%
      filter(!is.na(value), !is.na(year))
    
    df_firm <- bind_rows(df_firm, df_tmp)
  }
  cat("    read", length(txt_files), "txt files;",
      "years:", paste(sort(unique(str_extract(df_firm$name, "\\d{4}$"))), collapse = ", "), "\n")
  df_firm
}

# ---------------------------------------------------------------------------
# Count keywords for one firm's tidy doc -> (name, n_keyword, keyword).
# Mirrors countWords() in count_keyword_frequency_parallel.R:
# split by name (firm-year), sentence-tokenize, stri_count each keyword, sum.
# ---------------------------------------------------------------------------
count_one_firm <- function(df_doc) {
  
  df_sentence <- df_doc %>%
    mutate(value = stringr::str_replace(value, "\x0b", "")) %>%
    unnest_tokens(output = text, input = value, token = "sentences") %>%
    filter(!(is.na(name) & is.na(gvkey)), !is.na(text))
  
  dfs <- split(df_sentence, df_sentence$name)
  
  out <- tibble()
  for (nm in names(dfs)) {
    splited <- dfs[[nm]]
    df_keyword_n <- tibble()
    for (keyword in df_final_key$word) {
      n_keyword <- splited %>%
        mutate(text = tolower(text)) %>%
        pull(text) %>%
        map_dbl(~ stringi::stri_count(., regex = keyword)) %>%
        sum()
      df_keyword_n <- bind_rows(
        df_keyword_n,
        tibble(name = nm, n_keyword = n_keyword, keyword = keyword)
      )
    }
    out <- bind_rows(out, df_keyword_n)
  }
  out
}

# ---------------------------------------------------------------------------
# Run the eight firms ONE BY ONE (explicit calls, no loop over a NAICS code).
# ---------------------------------------------------------------------------
cat(">>> Bumi Resources\n")
fix_01 <- count_one_firm(read_one_firm("200864_Bumi Resources"))

cat(">>> China National Coal Group\n")
fix_02 <- count_one_firm(read_one_firm("282040_China National Coal Group"))

cat(">>> Coterra Energy\n")
fix_03 <- count_one_firm(read_one_firm("020548_Coterra Energy"))

cat(">>> Equinor\n")
fix_04 <- count_one_firm(read_one_firm("220546_Equinor"))

cat(">>> Gazprom\n")
fix_05 <- count_one_firm(read_one_firm("206454_Gazprom"))

cat(">>> Lukoil\n")
fix_06 <- count_one_firm(read_one_firm("206457_Lukoil"))

cat(">>> Oil & Natural Gas\n")
fix_07 <- count_one_firm(read_one_firm("208175_Oil & Natural Gas"))

cat(">>> Vale\n")
fix_08 <- count_one_firm(read_one_firm("209382_Vale"))

df_fixed <- bind_rows(fix_01, fix_02, fix_03, fix_04,
                      fix_05, fix_06, fix_07, fix_08)

cat("\n>>> New correct rows - distinct names:\n")
df_fixed %>% distinct(name) %>% arrange(name) %>% print(n = 200)

# ---------------------------------------------------------------------------
# Load existing RDS (untouched), drop the 8 fake-year rows, bind the new ones.
# Fake names matched EXACTLY so nothing else can be removed (e.g. not Valero).
# ---------------------------------------------------------------------------
df_old <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS21.RDS"))

fake_names <- c(
  "Bumi Resources_2008",
  "China National Coal Group_2040",
  "Coterra Energy_2054",
  "Equinor_2054",
  "Gazprom_2064",
  "Lukoil_2064",
  "Oil & Natural Gas_2081",
  "Vale_2093"
)

cat("\n>>> Fake-year names found in old RDS (expect all 8):\n")
print(intersect(fake_names, unique(df_old$name)))

df_clean   <- df_old %>% filter(!name %in% fake_names)
df_correct <- bind_rows(df_clean, df_fixed)

cat("\n>>> Rows: old =", nrow(df_old),
    "| after drop =", nrow(df_clean),
    "| after bind =", nrow(df_correct), "\n")

# ---------------------------------------------------------------------------
# Save as a NEW file. Original df_wordCount_NAICS21.RDS is left as-is.
# ---------------------------------------------------------------------------
out_path <- glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS21_correct.RDS")
write_rds(df_correct, out_path)
cat("\n>>> Saved:", out_path, "\n")

# ---------------------------------------------------------------------------
# Verification: the eight firms now carry proper yearly names.
# ---------------------------------------------------------------------------
cat("\n>>> Verification - eight firms in the corrected file:\n")
df_correct %>%
  filter(name %in% df_fixed$name) %>%
  distinct(name) %>%
  arrange(name) %>%
  print(n = 200)