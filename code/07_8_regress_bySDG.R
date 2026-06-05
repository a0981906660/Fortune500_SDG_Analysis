#' @title EXPLORATORY: per-SDG regressions on the mining sample
#' @description
#'   *** READ THIS BEFORE USING THE OUTPUT ***
#'   This script estimates ONE regression per SDG (SDG0..SDG16 -> up to 17
#'   regressions) for each dependent variable, on the mining sample only.
#'   It is EXPLORATORY, not confirmatory:
#'     - 17 separate tests per DV => multiple-comparison problem. At alpha=0.05
#'       roughly one "significant" coefficient is expected by chance alone.
#'     - Each SDG subset is SMALLER than the pooled sample, so power is lower
#'       and individual coefficients are unstable.
#'     - The aggregated three-index models (07_6 / 07_7 / 07_8) remain the
#'       CONFIRMATORY analysis. These per-SDG results should be reported as a
#'       descriptive / exploratory appendix, with an explicit statement that no
#'       single SDG coefficient is interpreted in isolation and that the family
#'       of tests is subject to multiple comparisons.
#'
#'   Data level:
#'     Uses the firm-year-SDG level colocation index
#'     (df_colocation_index_firm-year-SDG_level.RDS from 09_1), NOT the
#'     firm-year level file. For each SDG k we keep only firm-years that
#'     mention SDG k (n_keyword > 0 was already enforced in 09_1) and regress.
#'
#'   Specification (per SDG, mirrors the picks):
#'     y ~ Complementarity + Independence + Pressure + size control
#'         | year (+ NAICS2)
#'     DV    : ROA (log_emp), ROS (log_at), Tobin's Q (log_emp)
#'     SEs   : firm-level clustered; HC1 also available
#'   No country/region FE here (per your request: back to the no-geo-FE setup).

rm(list = ls()); gc()

library(fixest)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(glue)
source("./code/config.R")


# =============================================================================
# 1. Load firm-year-SDG level colocation index (the per-SDG file)
# =============================================================================
df_sdg <- read_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_colocation_index_firm-year-SDG_level.RDS")
)
cat("firm-year-SDG rows:", nrow(df_sdg),
    "| unique firms:", n_distinct(df_sdg$gvkey),
    "| SDGs present:", paste(sort(unique(df_sdg$sdg_number)), collapse = ","), "\n")


# =============================================================================
# 2. Load financial characteristics (same construction as 07_7)
# =============================================================================
firm_us <- read_csv(glue("{DROPBOX_PATH}/raw_data/compustat/comp_funda_us.csv"))
firm_global <- read_csv(glue("{DROPBOX_PATH}/raw_data/compustat/comp_funda_global.csv"))
df_fx <- read_csv(glue("{DROPBOX_PATH}/cleaned_data/exchange_rates_to_usd.csv"))

firm_global <- firm_global %>%
  select(gvkey, fyear, conm, curcd, revt, emp, at,
         roa, ros, tobin_q, naics, sic, datadate) %>%
  left_join(df_fx, join_by(closest(datadate <= datadate)))

firm_global_converted <- firm_global %>%
  mutate(across(c(revt, at), ~ case_when(
    curcd == "AUD" ~ . / AUD_USD, curcd == "BRL" ~ . / BRL_USD,
    curcd == "CHF" ~ . / CHF_USD, curcd == "CNY" ~ . / CNY_USD,
    curcd == "EUR" ~ . / EUR_USD, curcd == "GBP" ~ . / GBP_USD,
    curcd == "HKD" ~ . / HKD_USD, curcd == "INR" ~ . / INR_USD,
    curcd == "JPY" ~ . / JPY_USD, curcd == "KRW" ~ . / KRW_USD,
    curcd == "MXN" ~ . / MXN_USD, curcd == "MYR" ~ . / MYR_USD,
    curcd == "NOK" ~ . / NOK_USD, curcd == "RUB" ~ . / RUB_USD,
    curcd == "SAR" ~ . / SAR_USD, curcd == "SEK" ~ . / SEK_USD,
    curcd == "SGD" ~ . / SGD_USD, curcd == "THB" ~ . / THB_USD,
    curcd == "TRY" ~ . / TRY_USD, curcd == "TWD" ~ . / TWD_USD,
    curcd == "UGX" ~ . / UGX_USD, curcd == "USD" ~ . / 1,
    TRUE ~ NA_real_
  ))) %>%
  select(gvkey, fyear, conm, revt, emp, at, roa, ros, tobin_q, naics, sic)

firm_all <- firm_us %>%
  select(gvkey, fyear, conm, revt, emp, at, roa, ros, tobin_q, naics, sic) %>%
  full_join(firm_global_converted) %>%
  distinct() %>%
  group_by(gvkey, fyear) %>%
  mutate(dup_id = row_number()) %>%
  ungroup() %>%
  filter(dup_id == 1) %>%
  select(-dup_id) %>%
  mutate(
    naics2  = str_sub(as.character(naics), 1, 2),
    sic2    = str_sub(as.character(sic),   1, 2),
    log_emp = log(emp + 0.01),
    log_at  = log(at  + 0.01)
  )


# =============================================================================
# 3. Merge: firm-year-SDG index  x  firm-year financials
#    Join key: gvkey + (SDG file's year == financial fyear)
#    Result stays at firm-year-SDG level (one row per firm-year-SDG).
# =============================================================================
df_merged <- firm_all %>%
  inner_join(
    df_sdg %>%
      select(gvkey, year, sdg_number,
             n_keyword,
             n_complementary_keyword, n_independent_keyword, n_pressure_keyword,
             colocate_index_complementary,
             colocate_index_independent,
             colocate_index_pressure),
    by = c("gvkey" = "gvkey", "fyear" = "year")
  )


# =============================================================================
# 4. Restrict to mining (same definition as 07_6 / 07_7)
# =============================================================================
gvkeys_mining <- read_csv(
  glue("{DROPBOX_PATH}/raw_data/compustat/comp_funda_global.csv")
) %>% pull(gvkey) %>% unique()

df_mining <- df_merged %>% filter(gvkey %in% gvkeys_mining)

cat("\nMining firm-year-SDG rows:", nrow(df_mining),
    "| unique firms:", n_distinct(df_mining$gvkey), "\n")


# =============================================================================
# 5. Per-SDG sample sizes - INSPECT BEFORE TRUSTING ANY COEFFICIENT
#    Tiny n or few firms => that SDG's regression is not interpretable.
# =============================================================================
sdg_sizes <- df_mining %>%
  group_by(sdg_number) %>%
  summarise(
    firm_years = n(),
    firms      = n_distinct(gvkey),
    n_roa      = sum(!is.na(roa)),
    n_ros      = sum(!is.na(ros)),
    n_tobinq   = sum(!is.na(tobin_q)),
    .groups = "drop"
  ) %>%
  arrange(sdg_number)

cat("\n================ PER-SDG SAMPLE SIZES (mining) ================\n")
print(sdg_sizes, n = 30)
cat("Treat any SDG with very few firms / firm-years as descriptive only.\n")
cat("==============================================================\n")


# =============================================================================
# 6. Helper: run one DV across all SDGs, return a named list of models
# =============================================================================
.reg_dict <- c(
  fyear   = "Year dummies",
  year    = "Year dummies",
  naics2  = "Industry dummies (NAICS2)",
  colocate_index_complementary = "Complementarity Index",
  colocate_index_independent   = "Independence Index",
  colocate_index_pressure      = "Pressure Index",
  log_emp = "log(Employees)",
  log_at  = "log(Total Assets)"
)
TAB_DIR <- "./data/result/tables"

# Naming convention (consistent with 07_8):
#   tab_reg_<dv>_mining_sdg<k>_<se>.tex
#     <dv> roa|ros|tobinq   <k> 0..16   <se> cl|hc1
# Per-DV combined table across all SDGs:
#   tab_reg_<dv>_mining_persdg_all_<se>.tex

run_one_sdg <- function(k, dep, controls, data, vcov_type) {
  d <- data %>% filter(sdg_number == k, !is.na(.data[[dep]]))
  # need enough rows and at least 2 firms for clustering to be meaningful
  if (nrow(d) < 10 || n_distinct(d$gvkey) < 2) return(NULL)
  f <- as.formula(glue(
    "{dep} ~ colocate_index_complementary + colocate_index_independent +",
    " colocate_index_pressure + {controls} | fyear"
  ))
  if (vcov_type == "cl") {
    feols(f, cluster = ~ gvkey, data = d)
  } else {
    feols(f, vcov = "HC1", data = d)
  }
}

# Run all SDGs for one DV; print a combined table; save cl + hc1 combined tables
run_dv_all_sdgs <- function(dep, controls, tag, data) {
  ks <- sort(unique(data$sdg_number))
  
  models_cl  <- map(ks, ~ run_one_sdg(.x, dep, controls, data, "cl"))
  models_hc1 <- map(ks, ~ run_one_sdg(.x, dep, controls, data, "hc1"))
  names(models_cl)  <- glue("SDG{ks}")
  names(models_hc1) <- glue("SDG{ks}")
  
  # drop SDGs that returned NULL (too small)
  keep <- !map_lgl(models_cl, is.null)
  models_cl  <- models_cl[keep]
  models_hc1 <- models_hc1[keep]
  kept_labels <- names(models_cl)
  
  cat(glue("\n\n######## {tag}: per-SDG, mining, clustered ########\n"))
  cat("SDGs estimated:", paste(kept_labels, collapse = ", "), "\n")
  print(etable(models_cl, coefstat = "tstat", dict = .reg_dict,
               headers = kept_labels))
  
  # save combined tables (one column per SDG)
  etable(models_cl, coefstat = "tstat", dict = .reg_dict, headers = kept_labels,
         file = glue("{TAB_DIR}/tab_reg_{tag}_mining_persdg_all_cl.tex"),
         replace = TRUE)
  etable(models_hc1, coefstat = "tstat", dict = .reg_dict, headers = kept_labels,
         file = glue("{TAB_DIR}/tab_reg_{tag}_mining_persdg_all_hc1.tex"),
         replace = TRUE)
  
  # also save each SDG individually (clustered) for flexibility
  walk2(models_cl, kept_labels, function(m, lab) {
    kk <- str_remove(lab, "SDG")
    etable(m, coefstat = "tstat", dict = .reg_dict,
           file = glue("{TAB_DIR}/tab_reg_{tag}_mining_sdg{kk}_cl.tex"),
           replace = TRUE)
  })
  
  invisible(list(cl = models_cl, hc1 = models_hc1))
}


# =============================================================================
# 7. Run for the three DVs
#    ROA -> log_emp ; ROS -> log_at ; Tobin's Q -> log_emp
#    (matches the three picks in 07_7)
# =============================================================================
res_roa    <- run_dv_all_sdgs("roa",     "log_emp", "roa",
                              df_mining)
res_ros    <- run_dv_all_sdgs("ros",     "log_at",  "ros",
                              df_mining)
res_tobinq <- run_dv_all_sdgs("tobin_q", "log_emp", "tobinq",
                              df_mining %>% filter(!is.na(tobin_q)))


# =============================================================================
# 8. OPTIONAL: a Benjamini-Hochberg style false-discovery-rate adjustment
#    on the Complementarity Index p-values across SDGs, per DV.
#    This is the honest way to report a family of 17 tests: it tells you which
#    "significant" coefficients survive correction for multiple comparisons.
# =============================================================================
fdr_on_complementarity <- function(models, dv_label) {
  if (length(models) == 0) return(invisible(NULL))
  rows <- imap_dfr(models, function(m, lab) {
    ct <- tryCatch(coeftable(m), error = function(e) NULL)
    if (is.null(ct)) return(NULL)
    rn <- rownames(ct)
    i  <- which(rn == "colocate_index_complementary")
    if (length(i) == 0) return(NULL)
    tibble(sdg = lab,
           estimate = ct[i, 1],
           p_raw    = ct[i, 4])
  })
  if (nrow(rows) == 0) return(invisible(NULL))
  rows <- rows %>%
    mutate(p_BH = p.adjust(p_raw, method = "BH")) %>%
    arrange(p_raw)
  cat(glue("\n--- {dv_label}: Complementarity Index across SDGs, BH-adjusted ---\n"))
  print(rows, n = 30)
  invisible(rows)
}

cat("\n\n================ MULTIPLE-COMPARISON CHECK ================\n")
cat("Benjamini-Hochberg FDR on the Complementarity Index p-values.\n")
cat("A coefficient that is 'significant' raw but has p_BH > 0.05 does NOT\n")
cat("survive correction and should not be reported as a finding.\n")
fdr_on_complementarity(res_roa$cl,    "ROA (clustered)")
fdr_on_complementarity(res_ros$cl,    "ROS (clustered)")
fdr_on_complementarity(res_tobinq$cl, "Tobin's Q (clustered)")
cat("==========================================================\n")


cat("\n\nDone. Tables in", TAB_DIR, ":\n")
cat("  tab_reg_{roa,ros,tobinq}_mining_persdg_all_cl.tex   (all SDGs, one table)\n")
cat("  tab_reg_{roa,ros,tobinq}_mining_persdg_all_hc1.tex\n")
cat("  tab_reg_{roa,ros,tobinq}_mining_sdg<k>_cl.tex        (one per SDG)\n")
cat("\nReminder: report these as EXPLORATORY. Lead with the BH-adjusted check,\n")
cat("not the raw stars, and do not interpret any single SDG in isolation.\n")