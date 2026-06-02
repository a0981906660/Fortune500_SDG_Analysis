#' @title Regression: ROA / ROS / Tobin's Q on Complementarity Index
#' @description
#'   Dependent variables  : ROA, ROS, Tobin's Q
#'   Independent variable : n_complementary_keyword  (Complementarity Index)
#'   Control variables    : n_independent_keyword    (Independence Index)
#'                          n_pressure_keyword       (Pressure Index)
#'                          log(at)                  (firm size, log total assets)
#'   Fixed effects        : year, industry (NAICS2 and SIC2)
#'   Standard errors      : HC1 (heteroskedasticity-robust)
#'
#'   Changelog:
#'     - Removed `at` (total assets) from all regression models
#'     - Replaced `emp` with `log(emp)` as firm size control
#'     - Added `log(at)` back as an additional firm size control variable
#'     - Removed `log(emp)` from all regression models

# clean up environment
rm(list = ls()); gc()

library(fixest)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(glue)
source("./code/config.R")

# =============================================================================
# 1. Load colocation index data (firm-year level)
#    Source: 09_1_aggregate_firm-year-level_colocation_index.R
#    Key columns:
#      gvkey, name, year, naics,
#      n_keyword, n_complementary_keyword, n_independent_keyword,
#      n_pressure_keyword, n_noCIP_keyword,
#      colocate_index_complementary, colocate_index_independent,
#      colocate_index_pressure
# =============================================================================
df_colocation <- read_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_colocation_index_firm-year_level.RDS")
)

cat("Colocation data: ", nrow(df_colocation), "rows,",
    n_distinct(df_colocation$gvkey), "unique firms\n")
df_colocation %>% glimpse()


# =============================================================================
# 2. Load financial characteristics
#    Source: pull_firm_characteristics.py
#    Key columns: gvkey, fyear, conm, at, revt,
#                 roa, ros, tobin_q, naics, sic
#    Note: tobin_q is NULL for all global firms (prcc_f unavailable in
#          comp.g_funda). Only US-listed firms will have valid Tobin's Q.
# =============================================================================
firm_us <- read_csv(
  glue("{DROPBOX_PATH}/raw_data/compustat/comp_funda_us.csv")
)
firm_global <- read_csv(
  glue("{DROPBOX_PATH}/raw_data/compustat/comp_funda_global.csv")
)

df_fx <- read_csv(
  glue("{DROPBOX_PATH}/cleaned_data/exchange_rates_to_usd.csv")
)

# ---- Convert global firm financials to USD --------------------------------
firm_global <- firm_global %>%
  select(gvkey, fyear, conm, curcd, at, revt,
         roa, ros, tobin_q, naics, sic, datadate) %>%
  left_join(df_fx, join_by(closest(datadate <= datadate)))

firm_global_converted <- firm_global %>%
  mutate(across(c(at, revt), ~ case_when(
    curcd == "AUD" ~ . / AUD_USD,
    curcd == "BRL" ~ . / BRL_USD,
    curcd == "CHF" ~ . / CHF_USD,
    curcd == "CNY" ~ . / CNY_USD,
    curcd == "EUR" ~ . / EUR_USD,
    curcd == "GBP" ~ . / GBP_USD,
    curcd == "HKD" ~ . / HKD_USD,
    curcd == "INR" ~ . / INR_USD,
    curcd == "JPY" ~ . / JPY_USD,
    curcd == "KRW" ~ . / KRW_USD,
    curcd == "MXN" ~ . / MXN_USD,
    curcd == "MYR" ~ . / MYR_USD,
    curcd == "NOK" ~ . / NOK_USD,
    curcd == "RUB" ~ . / RUB_USD,
    curcd == "SAR" ~ . / SAR_USD,
    curcd == "SEK" ~ . / SEK_USD,
    curcd == "SGD" ~ . / SGD_USD,
    curcd == "THB" ~ . / THB_USD,
    curcd == "TRY" ~ . / TRY_USD,
    curcd == "TWD" ~ . / TWD_USD,
    curcd == "UGX" ~ . / UGX_USD,
    curcd == "USD" ~ . / 1,
    TRUE ~ NA_real_
  ))) %>%
  select(gvkey, fyear, conm, at, revt,
         roa, ros, tobin_q, naics, sic)


# ---- Stack US and global, remove duplicates --------------------------------
firm_all <- firm_us %>%
  select(gvkey, fyear, conm, at, revt,
         roa, ros, tobin_q, naics, sic) %>%
  full_join(firm_global_converted) %>%
  distinct() %>%
  group_by(gvkey, fyear) %>%
  mutate(dup_id = row_number()) %>%
  ungroup() %>%
  filter(dup_id == 1) %>%
  select(-dup_id) %>%
  mutate(
    naics2 = str_sub(as.character(naics), 1, 2),
    sic2   = str_sub(as.character(sic),   1, 2),
    # log(at): at is total assets in millions USD after currency conversion
    # add a small constant (0.01) before log to avoid log(0) = -Inf
    log_at = log(at + 0.01)
  )

cat("Financial data: ", nrow(firm_all), "rows,",
    n_distinct(firm_all$gvkey), "unique firms\n")


# =============================================================================
# 3. Merge colocation index with financial data
# =============================================================================
df_merged <- firm_all %>%
  inner_join(
    df_colocation %>%
      select(gvkey, year,
             n_keyword,
             n_complementary_keyword,
             n_independent_keyword,
             n_pressure_keyword,
             n_noCIP_keyword,
             colocate_index_complementary,
             colocate_index_independent,
             colocate_index_pressure),
    by = c("gvkey" = "gvkey", "fyear" = "year")
  )

cat("Merged data: ", nrow(df_merged), "rows,",
    n_distinct(df_merged$gvkey), "unique firms\n")

cat("Firms with valid Tobin's Q: ",
    df_merged %>% filter(!is.na(tobin_q)) %>% distinct(gvkey) %>% nrow(), "\n")
cat("Firms with valid ROA      : ",
    df_merged %>% filter(!is.na(roa))     %>% distinct(gvkey) %>% nrow(), "\n")
cat("Firms with valid ROS      : ",
    df_merged %>% filter(!is.na(ros))     %>% distinct(gvkey) %>% nrow(), "\n")
cat("Firms with valid log_at   : ",
    df_merged %>% filter(!is.na(log_at))  %>% distinct(gvkey) %>% nrow(), "\n")


# =============================================================================
# 4. Identify mining firms (for subsample analysis)
# =============================================================================
gvkeys_mining <- read_csv(
  glue("{DROPBOX_PATH}/raw_data/compustat/comp_funda_global.csv")
) %>% pull(gvkey) %>% unique()

df_merged <- df_merged %>%
  mutate(is_mining = if_else(gvkey %in% gvkeys_mining, 1L, 0L))


# =============================================================================
# 5. Helper: save regression table to LaTeX
# =============================================================================
save_reg_table <- function(reg_model, filename) {
  etable(
    reg_model,
    coefstat = "tstat",
    file     = glue("./data/result/tables/{filename}.tex"),
    replace  = TRUE
  )
}


# =============================================================================
# 6. Regressions
#
#   Formula structure (same across all dependent variables):
#
#     y ~ n_complementary_keyword        # main independent variable
#           + n_independent_keyword      # control: Independence Index
#           + n_pressure_keyword         # control: Pressure Index
#           + log_at                     # control: firm size (log total assets)
#           | csw0(fyear, naics2 / sic2) # stacked fixed effects
#
#   csw0(fyear, naics2) produces three nested models:
#     Model 1: no fixed effects
#     Model 2: year fixed effects only
#     Model 3: year + industry (NAICS2) fixed effects
#
#   Model families
#   --------------
#   A: Full sample, NAICS2 industry FE
#   B: Full sample, SIC2 industry FE
#   C: Mining subsample, NAICS2 industry FE
#   D: Mining subsample, SIC2 industry FE
# =============================================================================


# -----------------------------------------------------------------------------
# 6a. ROA as dependent variable
# -----------------------------------------------------------------------------
reg_roa_A <- feols(
  roa ~ colocate_index_complementary + colocate_index_independent + colocate_index_pressure
  + log_at | csw0(fyear, naics2),
  vcov = "HC1",
  data = df_merged
)

reg_roa_B <- feols(
  roa ~ colocate_index_complementary + colocate_index_independent + colocate_index_pressure
  + log_at | csw0(fyear, sic2),
  vcov = "HC1",
  data = df_merged
)

reg_roa_C <- feols(
  roa ~ colocate_index_complementary + colocate_index_independent + colocate_index_pressure
  + log_at | csw0(fyear, naics2),
  vcov = "HC1",
  data = df_merged %>% filter(is_mining == 1)
)

reg_roa_D <- feols(
  roa ~ colocate_index_complementary + colocate_index_independent + colocate_index_pressure
  + log_at | csw0(fyear, sic2),
  vcov = "HC1",
  data = df_merged %>% filter(is_mining == 1)
)

etable(reg_roa_A, coefstat = "tstat")
etable(reg_roa_B, coefstat = "tstat")
etable(reg_roa_C, coefstat = "tstat")
etable(reg_roa_D, coefstat = "tstat")

save_reg_table(reg_roa_A, "%tab_reg_roa_fullsample_naics2_logat")
save_reg_table(reg_roa_B, "%tab_reg_roa_fullsample_sic2_logat")
save_reg_table(reg_roa_C, "%tab_reg_roa_mining_naics2_logat")
save_reg_table(reg_roa_D, "%tab_reg_roa_mining_sic2_logat")

# -----------------------------------------------------------------------------
# 6b. ROS as dependent variable
# -----------------------------------------------------------------------------
reg_ros_A <- feols(
  ros ~ colocate_index_complementary + colocate_index_independent + colocate_index_pressure
  + log_at | csw0(fyear, naics2),
  vcov = "HC1",
  data = df_merged
)

reg_ros_B <- feols(
  ros ~ colocate_index_complementary + colocate_index_independent + colocate_index_pressure
  + log_at | csw0(fyear, sic2),
  vcov = "HC1",
  data = df_merged
)

reg_ros_C <- feols(
  ros ~ colocate_index_complementary + colocate_index_independent + colocate_index_pressure
  + log_at | csw0(fyear, naics2),
  vcov = "HC1",
  data = df_merged %>% filter(is_mining == 1)
)

reg_ros_D <- feols(
  ros ~ colocate_index_complementary + colocate_index_independent + colocate_index_pressure
  + log_at | csw0(fyear, sic2),
  vcov = "HC1",
  data = df_merged %>% filter(is_mining == 1)
)

etable(reg_ros_A, coefstat = "tstat")
etable(reg_ros_B, coefstat = "tstat")
etable(reg_ros_C, coefstat = "tstat")
etable(reg_ros_D, coefstat = "tstat")

save_reg_table(reg_ros_A, "%tab_reg_ros_fullsample_naics2_logat")
save_reg_table(reg_ros_B, "%tab_reg_ros_fullsample_sic2_logat")
save_reg_table(reg_ros_C, "%tab_reg_ros_mining_naics2_logat")
save_reg_table(reg_ros_D, "%tab_reg_ros_mining_sic2_logat")

# -----------------------------------------------------------------------------
# 6c. Tobin's Q as dependent variable
#     NOTE: tobin_q is only available for US-listed firms (prcc_f is NULL
#     for all global firms in comp.g_funda). The effective sample here will
#     therefore be US firms only even if df_merged contains global firms.
#     This is a known data limitation — flag it clearly in the thesis.
# -----------------------------------------------------------------------------
reg_tobinq_A <- feols(
  tobin_q ~ colocate_index_complementary + colocate_index_independent + colocate_index_pressure
  + log_at | csw0(fyear, naics2),
  vcov = "HC1",
  data = df_merged %>% filter(!is.na(tobin_q))
)

reg_tobinq_B <- feols(
  tobin_q ~ colocate_index_complementary + colocate_index_independent + colocate_index_pressure
  + log_at | csw0(fyear, sic2),
  vcov = "HC1",
  data = df_merged %>% filter(!is.na(tobin_q))
)

reg_tobinq_C <- feols(
  tobin_q ~ colocate_index_complementary + colocate_index_independent + colocate_index_pressure
  + log_at | csw0(fyear, naics2),
  vcov = "HC1",
  data = df_merged %>% filter(is_mining == 1, !is.na(tobin_q))
)

reg_tobinq_D <- feols(
  tobin_q ~ colocate_index_complementary + colocate_index_independent + colocate_index_pressure
  + log_at | csw0(fyear, sic2),
  vcov = "HC1",
  data = df_merged %>% filter(is_mining == 1, !is.na(tobin_q))
)

etable(reg_tobinq_A, coefstat = "tstat")
etable(reg_tobinq_B, coefstat = "tstat")
etable(reg_tobinq_C, coefstat = "tstat")
etable(reg_tobinq_D, coefstat = "tstat")

save_reg_table(reg_tobinq_A, "%tab_reg_tobinq_fullsample_naics2_logat")
save_reg_table(reg_tobinq_B, "%tab_reg_tobinq_fullsample_sic2_logat")
save_reg_table(reg_tobinq_C, "%tab_reg_tobinq_mining_naics2_logat")
save_reg_table(reg_tobinq_D, "%tab_reg_tobinq_mining_sic2_logat")

# =============================================================================
# 7. Combined display tables
# =============================================================================
etable(reg_roa_A, reg_roa_C, coefstat = "tstat",
       headers = c("Full sample", "Mining only"))

etable(reg_ros_A, reg_ros_C, coefstat = "tstat",
       headers = c("Full sample", "Mining only"))

etable(reg_tobinq_A, reg_tobinq_C, coefstat = "tstat",
       headers = c("Full sample (US only)", "Mining only (US only)"))

save_reg_table(list(reg_roa_A,    reg_roa_C),    "%tab_reg_roa_combined_logat")
save_reg_table(list(reg_ros_A,    reg_ros_C),    "%tab_reg_ros_combined_logat")
save_reg_table(list(reg_tobinq_A, reg_tobinq_C), "%tab_reg_tobinq_combined_logat")

# =============================================================================
# 8. Save merged dataset for further use
# =============================================================================
df_merged %>%
  write_rds(glue("{DROPBOX_PATH}/cleaned_data/regression_data_CIP_financial.RDS"))

cat("\nAll done. Tables saved to ./data/result/tables/\n")