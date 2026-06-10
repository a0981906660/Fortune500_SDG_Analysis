#' @title 02_6_rerun_eight_firms_CIP.R  (STANDALONE - edits no other script)
#' @description
#'   Companion to 02_5. The CIP co-location word-count file
#'   df_colocation_CIP_wordcount_NAICS21.RDS has the SAME eight firms collapsed
#'   into one fake-year row each (gvkey-as-year bug in readReports), e.g.
#'   Vale_2093 = the CIP counts summed over all of Vale's years.
#'
#'   This re-reads ONLY those eight folders, parses year correctly from the
#'   FILE NAME, recomputes the CIP co-location counts using the SAME logic as
#'   compute_colocation_complementary_index.R (window_size = 10, same CIP
#'   keyword lists, same sentence tokenisation), SEQUENTIALLY (no parallel),
#'   then:
#'     1. loads existing df_colocation_CIP_wordcount_NAICS21.RDS  (left on disk)
#'     2. drops the eight fake-year rows (matched by exact name)
#'     3. binds the freshly recomputed correct rows
#'     4. writes df_colocation_CIP_wordcount_NAICS21_correct.RDS   (NEW file)
#'
#'   Original df_colocation_CIP_wordcount_NAICS21.RDS is NEVER overwritten.
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
# Keyword inputs (identical sources to compute_colocation_complementary_index.R)
# ---------------------------------------------------------------------------
df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.RDS"))

index_keyword_list <- getComplementaryIndexKeywords(
  "./data/raw_data/keyword/Complementarity_independence_pressure_keywords.xlsx"
)
colocation_complementary_keywords <- index_keyword_list[["complementary"]]
colocation_independent_keywords   <- index_keyword_list[["independent"]]
colocation_pressure_keywords      <- index_keyword_list[["pressure"]]

REPORT_ROOT <- glue("{DROPBOX_PATH}/raw_data/Fortune_500_report")

# ---------------------------------------------------------------------------
# countColocationWords(): copied verbatim from
# compute_colocation_complementary_index.R (window_size = 10).
# Logging args dropped (not needed for a sequential one-off).
# ---------------------------------------------------------------------------
countColocationWords <- function(splited_data,
                                 sdg_keywords,
                                 comp_keywords,
                                 ind_keywords,
                                 press_keywords,
                                 window_size = 10) {
  
  comp_regex  <- paste(comp_keywords,  collapse = "|")
  ind_regex   <- paste(ind_keywords,   collapse = "|")
  press_regex <- paste(press_keywords, collapse = "|")
  
  name <- splited_data$name[1]
  
  df_words <- splited_data %>%
    mutate(sentence_id = row_number()) %>%
    unnest_tokens(output = word, input = text, token = "words", to_lower = TRUE) %>%
    group_by(sentence_id) %>%
    mutate(word_id = row_number()) %>%
    ungroup()
  
  final_counts <- tibble()
  
  for (sdg_keyword in sdg_keywords) {
    sentences_with_keyword <- splited_data %>%
      mutate(sentence_id = row_number()) %>%
      filter(stringi::stri_detect(text, regex = sdg_keyword, case_insensitive = TRUE))
    
    n_keyword_total <- sum(stringi::stri_count(
      sentences_with_keyword$text, regex = sdg_keyword, case_insensitive = TRUE))
    
    if (n_keyword_total == 0) {
      keyword_counts_tmp <- tibble(
        name = name, keyword = sdg_keyword,
        n_keyword = 0, n_complementary = 0,
        n_independent = 0, n_pressure = 0, n_no_cip = 0
      )
      final_counts <- bind_rows(final_counts, keyword_counts_tmp)
      next
    }
    
    classifications <- c()
    
    for (id in sentences_with_keyword$sentence_id) {
      sentence_text  <- tolower(sentences_with_keyword$text[sentences_with_keyword$sentence_id == id])
      sentence_words <- df_words %>% filter(sentence_id == id) %>% pull(word)
      
      keyword_occurrences <- stringi::stri_locate_all(
        sentence_text, regex = sdg_keyword, case_insensitive = TRUE)[[1]]
      
      for (i in 1:nrow(keyword_occurrences)) {
        pre_text  <- tolower(stringi::stri_sub(sentence_text, 1, keyword_occurrences[i, "start"] - 1))
        post_text <- tolower(stringi::stri_sub(sentence_text, keyword_occurrences[i, "end"] + 1, -1))
        
        pre_window  <- tail(stringi::stri_extract_all(pre_text,  regex = "\\w+")[[1]], window_size)
        post_window <- head(stringi::stri_extract_all(post_text, regex = "\\w+")[[1]], window_size)
        
        window_text <- paste(c(pre_window, post_window), collapse = " ")
        
        is_comp  <- stringi::stri_detect(window_text, regex = comp_regex)
        is_ind   <- stringi::stri_detect(window_text, regex = ind_regex)
        is_press <- stringi::stri_detect(window_text, regex = press_regex)
        
        classifications <- c(classifications, list(c(comp = is_comp, ind = is_ind, press = is_press)))
      }
    }
    
    n_comp  <- sum(sapply(classifications, `[`, "comp"))
    n_ind   <- sum(sapply(classifications, `[`, "ind"))
    n_press <- sum(sapply(classifications, `[`, "press"))
    n_none  <- sum(sapply(classifications, function(x) !any(x)))
    
    keyword_counts_tmp <- tibble(
      name = name, keyword = sdg_keyword,
      n_keyword = n_keyword_total,
      n_complementary = n_comp, n_independent = n_ind,
      n_pressure = n_press, n_no_cip = n_none
    )
    final_counts <- bind_rows(final_counts, keyword_counts_tmp)
  }
  return(final_counts)
}

# ---------------------------------------------------------------------------
# Read one firm folder -> tidy (value, name, gvkey, year), year from basename.
# name = "{display}_{year}" exactly as readReports() builds it.
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
  cat("    read", length(txt_files), "txt;",
      "years:", paste(sort(unique(str_extract(df_firm$name, "\\d{4}$"))), collapse = ", "), "\n")
  df_firm
}

# ---------------------------------------------------------------------------
# CIP-count one firm's doc -> 7-col tibble, SEQUENTIAL over its firm-years.
# Pre-processing (\x0b strip, sentence tokenise, filter) matches
# compute_colocation_complementary_index.R exactly.
# ---------------------------------------------------------------------------
cip_one_firm <- function(df_doc) {
  
  df_sentence <- df_doc %>%
    mutate(value = stringr::str_replace(value, "\x0b", "")) %>%
    unnest_tokens(output = text, input = value, token = "sentences") %>%
    filter(!is.na(gvkey) | !is.na(name)) %>%
    arrange(gvkey)
  
  dfs <- split(df_sentence, df_sentence$name)
  
  out <- tibble()
  for (nm in names(dfs)) {
    cat("      CIP:", nm, "\n")
    res <- countColocationWords(
      splited_data   = dfs[[nm]],
      sdg_keywords   = df_final_key$word,
      comp_keywords  = colocation_complementary_keywords,
      ind_keywords   = colocation_independent_keywords,
      press_keywords = colocation_pressure_keywords,
      window_size    = 10
    )
    out <- bind_rows(out, res)
  }
  out
}

# ---------------------------------------------------------------------------
# Run the eight firms ONE BY ONE (explicit calls, no loop over a NAICS code).
# ---------------------------------------------------------------------------
cat(">>> Bumi Resources\n");            fix_01 <- cip_one_firm(read_one_firm("200864_Bumi Resources"))
cat(">>> China National Coal Group\n"); fix_02 <- cip_one_firm(read_one_firm("282040_China National Coal Group"))
cat(">>> Coterra Energy\n");            fix_03 <- cip_one_firm(read_one_firm("020548_Coterra Energy"))
cat(">>> Equinor\n");                   fix_04 <- cip_one_firm(read_one_firm("220546_Equinor"))
cat(">>> Gazprom\n");                   fix_05 <- cip_one_firm(read_one_firm("206454_Gazprom"))
cat(">>> Lukoil\n");                    fix_06 <- cip_one_firm(read_one_firm("206457_Lukoil"))
cat(">>> Oil & Natural Gas\n");         fix_07 <- cip_one_firm(read_one_firm("208175_Oil & Natural Gas"))
cat(">>> Vale\n");                      fix_08 <- cip_one_firm(read_one_firm("209382_Vale"))

df_fixed <- bind_rows(fix_01, fix_02, fix_03, fix_04,
                      fix_05, fix_06, fix_07, fix_08)

cat("\n>>> New correct rows - distinct names:\n")
df_fixed %>% distinct(name) %>% arrange(name) %>% print(n = 200)

# ---------------------------------------------------------------------------
# Load existing CIP file (untouched), drop the 8 fake-year rows, bind new.
# ---------------------------------------------------------------------------
df_old <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_colocation_CIP_wordcount_NAICS21.RDS"))

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

cat("\n>>> Fake-year names found in old CIP file (expect all 8):\n")
print(intersect(fake_names, unique(df_old$name)))

# guard: new rows must have the same 7 columns, same order, before binding
stopifnot(identical(names(df_old), names(df_fixed)))

df_clean   <- df_old %>% filter(!name %in% fake_names)
df_correct <- bind_rows(df_clean, df_fixed)

cat("\n>>> Rows: old =", nrow(df_old),
    "| after drop =", nrow(df_clean),
    "| after bind =", nrow(df_correct), "\n")

# ---------------------------------------------------------------------------
# Save as NEW file. Original is left as-is.
# ---------------------------------------------------------------------------
out_path <- glue("{DROPBOX_PATH}/cleaned_data/df_colocation_CIP_wordcount_NAICS21_correct.RDS")
write_rds(df_correct, out_path)
cat("\n>>> Saved:", out_path, "\n")

# verification
cat("\n>>> Verification - eight firms in the corrected file:\n")
df_correct %>%
  filter(name %in% df_fixed$name) %>%
  distinct(name) %>%
  arrange(name) %>%
  print(n = 200)