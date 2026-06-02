#' @title Regression: Three selected models with firm-level clustered SEs
#' @description
#'   This script focuses on the THREE picks selected for the revised thesis:
#'
#'     Pick 1 - Tobin's Q  ~ colocation indices + log(emp)
#'     Pick 2 - ROA        ~ colocation indices + log(emp)
#'     Pick 3 - ROS        ~ colocation indices + log(at)
#'
#'   Each pick is estimated under TWO standard-error specifications so that
#'   the difference between heteroskedasticity-robust SEs (the original
#'   reporting) and firm-level clustered SEs is transparent:
#'
#'     - vcov = "HC1"          : heteroskedasticity-robust (original)
#'     - cluster = ~ gvkey     : firm-level clustered (resubmission)
#'
#'   Year and industry (NAICS2) are included as DUMMY VARIABLES via csw0().
#'   Firm-level fixed effects are NOT estimated. Terminology is updated
#'   accordingly in the output labels.
#'
#'   Sample design:
#'     A: Full sample (Fortune Global 500 + mining)        - cols 1-3 in tables
#'     C: Mining subsample only (NAICS 21)                 - cols 4-6 in tables
#'   Note: the column 6 specification (mining + NAICS2 dummies) is mechanically
#'   redundant when the sample is already restricted to NAICS 21. It is kept
#'   here for completeness but should be flagged in the thesis.
#'
#'   Changelog vs. 07_6 scripts:
#'     - Restricted to the three picks only
#'     - Added firm-level clustered standard errors
#'     - Renamed FE labels to make clear they are dummy variables, not panel FE
#'     - SIC2 and unused subsamples dropped to keep the file lean

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
# =============================================================================
df_colocation <- read_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_colocation_index_firm-year_level.RDS")
)

cat("Colocation data: ", nrow(df_colocation), "rows,",
    n_distinct(df_colocation$gvkey), "unique firms\n")


# =============================================================================
# 2. Load financial characteristics
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
  select(gvkey, fyear, conm, curcd, revt, emp, at,
         roa, ros, tobin_q, naics, sic, datadate) %>%
  left_join(df_fx, join_by(closest(datadate <= datadate)))

firm_global_converted <- firm_global %>%
  mutate(across(c(revt, at), ~ case_when(
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
  select(gvkey, fyear, conm, revt, emp, at,
         roa, ros, tobin_q, naics, sic)


# ---- Stack US and global, remove duplicates --------------------------------
firm_all <- firm_us %>%
  select(gvkey, fyear, conm, revt, emp, at,
         roa, ros, tobin_q, naics, sic) %>%
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


# =============================================================================
# 4. Identify mining firms (for subsample analysis)
# =============================================================================
gvkeys_mining <- read_csv(
  glue("{DROPBOX_PATH}/raw_data/compustat/comp_funda_global.csv")
) %>% pull(gvkey) %>% unique()

df_merged <- df_merged %>%
  mutate(is_mining = if_else(gvkey %in% gvkeys_mining, 1L, 0L))


# =============================================================================
# 5. Helpers
# =============================================================================

# Shared dictionary used across both helpers
.reg_dict <- c(
  fyear  = "Year dummies",
  naics2 = "Industry dummies (NAICS2)",
  colocate_index_complementary = "Complementarity Index",
  colocate_index_independent   = "Independence Index",
  colocate_index_pressure      = "Pressure Index",
  log_emp = "log(Employees)",
  log_at  = "log(Total Assets)"
)

# Save regression table to LaTeX with relabelled "fixed effects" rows
# `reg_model` can be a single fixest model OR a list of models (e.g. from csw0)
save_reg_table <- function(reg_model, filename) {
  etable(
    reg_model,
    coefstat = "tstat",
    dict     = .reg_dict,
    file     = glue("./data/result/tables/{filename}.tex"),
    replace  = TRUE
  )
}

# Print to console with the same relabelling.
# `reg_model` can be a single fixest model OR a list of models (e.g. from csw0).
show_reg_table <- function(reg_model) {
  etable(
    reg_model,
    coefstat = "tstat",
    dict     = .reg_dict
  )
}


# =============================================================================
# 6. PICK 1 - Tobin's Q with log(emp)
# =============================================================================

# ---- 6a. HC1 (original) ----------------------------------------------------
reg_tobinq_full_HC1 <- feols(
  tobin_q ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp |
    csw0(fyear, naics2),
  vcov = "HC1",
  data = df_merged %>% filter(!is.na(tobin_q))
)

reg_tobinq_mining_HC1 <- feols(
  tobin_q ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp |
    csw0(fyear, naics2),
  vcov = "HC1",
  data = df_merged %>% filter(is_mining == 1, !is.na(tobin_q))
)

# ---- 6b. Firm-level clustered (resubmission) -------------------------------
reg_tobinq_full_CL <- feols(
  tobin_q ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp |
    csw0(fyear, naics2),
  cluster = ~ gvkey,
  data = df_merged %>% filter(!is.na(tobin_q))
)

reg_tobinq_mining_CL <- feols(
  tobin_q ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp |
    csw0(fyear, naics2),
  cluster = ~ gvkey,
  data = df_merged %>% filter(is_mining == 1, !is.na(tobin_q))
)

cat("\n========================================================\n")
cat("PICK 1: Tobin's Q with log(emp)\n")
cat("========================================================\n")
cat("\n--- HC1 (original) - Full sample ---\n")
show_reg_table(reg_tobinq_full_HC1)
cat("\n--- Firm-clustered - Full sample ---\n")
show_reg_table(reg_tobinq_full_CL)
cat("\n--- HC1 (original) - Mining only ---\n")
show_reg_table(reg_tobinq_mining_HC1)
cat("\n--- Firm-clustered - Mining only ---\n")
show_reg_table(reg_tobinq_mining_CL)

save_reg_table(reg_tobinq_full_CL,
               "%tab_reg_tobinq_fullsample_logemp_clustered")
save_reg_table(reg_tobinq_mining_CL,
               "%tab_reg_tobinq_mining_logemp_clustered")


# =============================================================================
# 7. PICK 2 - ROA with log(emp)
# =============================================================================

# ---- 7a. HC1 (original) ----------------------------------------------------
reg_roa_full_HC1 <- feols(
  roa ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp |
    csw0(fyear, naics2),
  vcov = "HC1",
  data = df_merged
)

reg_roa_mining_HC1 <- feols(
  roa ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp |
    csw0(fyear, naics2),
  vcov = "HC1",
  data = df_merged %>% filter(is_mining == 1)
)

# ---- 7b. Firm-level clustered (resubmission) -------------------------------
reg_roa_full_CL <- feols(
  roa ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp |
    csw0(fyear, naics2),
  cluster = ~ gvkey,
  data = df_merged
)

reg_roa_mining_CL <- feols(
  roa ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp |
    csw0(fyear, naics2),
  cluster = ~ gvkey,
  data = df_merged %>% filter(is_mining == 1)
)

cat("\n========================================================\n")
cat("PICK 2: ROA with log(emp)\n")
cat("========================================================\n")
cat("\n--- HC1 (original) - Full sample ---\n")
show_reg_table(reg_roa_full_HC1)
cat("\n--- Firm-clustered - Full sample ---\n")
show_reg_table(reg_roa_full_CL)
cat("\n--- HC1 (original) - Mining only ---\n")
show_reg_table(reg_roa_mining_HC1)
cat("\n--- Firm-clustered - Mining only ---\n")
show_reg_table(reg_roa_mining_CL)

save_reg_table(reg_roa_full_CL,
               "%tab_reg_roa_fullsample_logemp_clustered")
save_reg_table(reg_roa_mining_CL,
               "%tab_reg_roa_mining_logemp_clustered")


# =============================================================================
# 8. PICK 3 - ROS with log(at)
# =============================================================================

# ---- 8a. HC1 (original) ----------------------------------------------------
reg_ros_full_HC1 <- feols(
  ros ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_at |
    csw0(fyear, naics2),
  vcov = "HC1",
  data = df_merged
)

reg_ros_mining_HC1 <- feols(
  ros ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_at |
    csw0(fyear, naics2),
  vcov = "HC1",
  data = df_merged %>% filter(is_mining == 1)
)

# ---- 8b. Firm-level clustered (resubmission) -------------------------------
reg_ros_full_CL <- feols(
  ros ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_at |
    csw0(fyear, naics2),
  cluster = ~ gvkey,
  data = df_merged
)

reg_ros_mining_CL <- feols(
  ros ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_at |
    csw0(fyear, naics2),
  cluster = ~ gvkey,
  data = df_merged %>% filter(is_mining == 1)
)

cat("\n========================================================\n")
cat("PICK 3: ROS with log(at)\n")
cat("========================================================\n")
cat("\n--- HC1 (original) - Full sample ---\n")
show_reg_table(reg_ros_full_HC1)
cat("\n--- Firm-clustered - Full sample ---\n")
show_reg_table(reg_ros_full_CL)
cat("\n--- HC1 (original) - Mining only ---\n")
show_reg_table(reg_ros_mining_HC1)
cat("\n--- Firm-clustered - Mining only ---\n")
show_reg_table(reg_ros_mining_CL)

save_reg_table(reg_ros_full_CL,
               "%tab_reg_ros_fullsample_logat_clustered")
save_reg_table(reg_ros_mining_CL,
               "%tab_reg_ros_mining_logat_clustered")


# =============================================================================
# 9. Side-by-side comparison: HC1 vs firm-clustered
#    Quick visual check on which significance levels survive clustering
# =============================================================================

cat("\n\n========================================================\n")
cat("SIDE-BY-SIDE: HC1 vs firm-clustered (full vs mining)\n")
cat("Each panel shows model 3 (year + NAICS2 dummies)\n")
cat("========================================================\n")

# Pick 1 - Tobin's Q
cat("\n--- Pick 1: Tobin's Q ---\n")
etable(
  reg_tobinq_full_HC1[[3]],  reg_tobinq_full_CL[[3]],
  reg_tobinq_mining_HC1[[3]], reg_tobinq_mining_CL[[3]],
  coefstat = "tstat",
  headers  = c("Full (HC1)", "Full (Clust)", "Mining (HC1)", "Mining (Clust)"),
  dict     = .reg_dict
)

# Pick 2 - ROA
cat("\n--- Pick 2: ROA ---\n")
etable(
  reg_roa_full_HC1[[3]],  reg_roa_full_CL[[3]],
  reg_roa_mining_HC1[[3]], reg_roa_mining_CL[[3]],
  coefstat = "tstat",
  headers  = c("Full (HC1)", "Full (Clust)", "Mining (HC1)", "Mining (Clust)"),
  dict     = .reg_dict
)

# Pick 3 - ROS
cat("\n--- Pick 3: ROS ---\n")
etable(
  reg_ros_full_HC1[[3]],  reg_ros_full_CL[[3]],
  reg_ros_mining_HC1[[3]], reg_ros_mining_CL[[3]],
  coefstat = "tstat",
  headers  = c("Full (HC1)", "Full (Clust)", "Mining (HC1)", "Mining (Clust)"),
  dict     = .reg_dict
)

save_reg_table(
  list(reg_tobinq_full_HC1[[3]], reg_tobinq_full_CL[[3]],
       reg_tobinq_mining_HC1[[3]], reg_tobinq_mining_CL[[3]]),
  "%tab_reg_tobinq_HC1_vs_clustered"
)
save_reg_table(
  list(reg_roa_full_HC1[[3]], reg_roa_full_CL[[3]],
       reg_roa_mining_HC1[[3]], reg_roa_mining_CL[[3]]),
  "%tab_reg_roa_HC1_vs_clustered"
)
save_reg_table(
  list(reg_ros_full_HC1[[3]], reg_ros_full_CL[[3]],
       reg_ros_mining_HC1[[3]], reg_ros_mining_CL[[3]]),
  "%tab_reg_ros_HC1_vs_clustered"
)


cat("\n\nAll done. Tables saved to ./data/result/tables/\n")
cat("Filenames end with '_clustered' to distinguish from previous output.\n")

