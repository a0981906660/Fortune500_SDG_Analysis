#' @title Regression: ROA / ROS / Tobin's Q on the Complementarity Index
#' @description
#'   Dependent variables : ROA, ROS, Tobin's Q
#'   Independent variable: Complementarity Index (colocate_index_complementary)
#'   Control variables   : Independence Index (colocate_index_independent)
#'                         Pressure Index     (colocate_index_pressure)
#'                         log(emp)           (firm size, log employees)
#'   Industry            : project NAICS2 classification (naics_coloc)
#'   Fixed effects       : year, industry
#'   Standard errors     : HC1 (heteroskedasticity-robust)
#'
#'   Models per dependent variable
#'   -----------------------------
#'     Full sample : ONE table, 4 nested columns
#'                   no FE | +Year | +Year+Industry | +Year+Industry+Country
#'                   [csw0(fyear, naics_coloc, country)]
#'     Mining (21) : ONE table, 3 nested columns
#'                   no FE | +Year | +Year+Country
#'                   [csw0(fyear, country)]
#'   The country column uses the HQ country from the master sheet (step 3b),
#'   NOT the ranking-frame "region" (region is far coarser and partly proxies
#'   firm size). Country is simply the right-most column of each table.
#'   CAUTION on the mining country column: the 63 mining firms span 21 countries
#'   with 15 single-firm, so that country FE behaves largely like a firm fixed
#'   effect (and for Tobin's Q mining there are only 5 countries, ~86% USA);
#'   the index coefficients in that column are identified mainly within the USA
#'   and Indonesia, so it is fragile and should be read with care.
#'
#'   Two corrections relative to the earlier 07_6 scripts
#'   -----------------------------------------------------
#'   FIX 1  gvkey type. The colocation file stores gvkey as a zero-padded
#'          character (e.g. "001380"); the Compustat CSVs store it as an
#'          integer (e.g. 2410). The earlier inner_join therefore risked
#'          silently dropping rows, or erroring on newer dplyr. Both sides are
#'          now cast to integer with as.integer(as.character(gvkey)) before the
#'          join, which strips the zero padding consistently on both sides.
#'
#'   FIX 2  mining definition. The earlier scripts used
#'             gvkeys_mining <- read_csv(comp_funda_global.csv) %>% pull(gvkey)
#'             is_mining     <- gvkey %in% gvkeys_mining
#'          but comp_funda_global.csv is every NON-US-listed firm pulled for the
#'          full master gvkey list (331 firms spanning 17 industries), not a
#'          mining list. That flag wrongly included 217 non-mining firms and
#'          wrongly excluded 27 US-listed miners. Mining is now defined from the
#'          project NAICS2 classification carried in from the colocation file:
#'             is_mining <- naics_coloc == 21
#'
#'   Note on currency. ROA, ROS and Tobin's Q are unit-free ratios and log(emp)
#'   is a head-count, so none of the regression variables need currency
#'   conversion. The FX / case_when block that the earlier scripts carried over
#'   only affected the monetary levels at / revt, which are not used here, so it
#'   has been removed. Re-add it only if a monetary variable is reintroduced.

# clean up environment
rm(list = ls()); gc()

library(fixest)
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(glue)
source("./code/config.R")


# =============================================================================
# 1. Colocation index data (firm-year level)  [from 09_1]
#    FIX 1: cast gvkey to integer so it matches the financial side.
#    `naics` here is the project NAICS2 classification (the loop's NAICS2_CODE),
#    the same industry classification used in the heatmaps and time trends.
# =============================================================================
df_colocation <- read_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_colocation_index_firm-year_level.RDS")
) %>%
  mutate(gvkey = as.integer(as.character(gvkey)))

cat("Colocation rows:", nrow(df_colocation),
    "| firms with gvkey:",
    n_distinct(df_colocation$gvkey[!is.na(df_colocation$gvkey)]),
    "| firm-years with NA gvkey (dropped by the join):",
    sum(is.na(df_colocation$gvkey)), "\n")


# =============================================================================
# 2. Financial characteristics  [from pull_firm_characteristics.py]
#    roa, ros, tobin_q are pre-computed ratios in the CSVs.
#    tobin_q is NA for all global firms (prcc_f is NULL in comp.g_funda), so the
#    Tobin's Q models are effectively US-listed firms only.
#    FIX 1: cast gvkey to integer.
# =============================================================================
sel_cols <- c("gvkey", "fyear", "conm", "emp",
              "roa", "ros", "tobin_q", "naics")

firm_us <- read_csv(
  glue("{DROPBOX_PATH}/raw_data/compustat/comp_funda_us.csv")
) %>% select(all_of(sel_cols))

firm_global <- read_csv(
  glue("{DROPBOX_PATH}/raw_data/compustat/comp_funda_global.csv")
) %>% select(all_of(sel_cols))

firm_all <- bind_rows(firm_us, firm_global) %>%
  distinct() %>%
  group_by(gvkey, fyear) %>%
  mutate(dup_id = row_number()) %>%
  ungroup() %>%
  filter(dup_id == 1) %>%
  select(-dup_id) %>%
  mutate(
    gvkey   = as.integer(as.character(gvkey)),
    # Compustat naicsh, kept only for the optional industry-FE swap below
    naics2  = str_sub(as.character(naics), 1, 2),
    # emp is in thousands of employees; +0.01 avoids log(0) = -Inf
    log_emp = log(emp + 0.01)
  )

cat("Financial rows:", nrow(firm_all),
    "| unique firms:", n_distinct(firm_all$gvkey), "\n")


# =============================================================================
# 3. Merge colocation index with financial data
#    FIX 2 prep: carry the project NAICS2 in as `naics_coloc`.
# =============================================================================
df_merged <- firm_all %>%
  inner_join(
    df_colocation %>%
      select(gvkey, year,
             naics_coloc = naics,
             colocate_index_complementary,
             colocate_index_independent,
             colocate_index_pressure),
    by = c("gvkey" = "gvkey", "fyear" = "year")
  )

cat("Merged rows:", nrow(df_merged),
    "| unique firms:", n_distinct(df_merged$gvkey), "\n")
# sanity: expect about 3582 rows / 388 firms.
# if this collapses to roughly 1753 rows / 198 firms, the gvkey types failed
# to harmonise and the join is still broken.


# =============================================================================
# 3b. Attach headquarters country from the master sheet  [pattern from 07_9]
#     Source : company_reference_master.xlsx, sheet "master", column `Country`
#              = HQ country (2-letter codes; 0 NAs on the master sheet).
#     gvkey is cast to integer to match the financial / colocation sides.
#     The one duplicate gvkey on the master (101846 = Sumitomo / Sumitomo Life,
#     both JP) is collapsed with slice(1); country is unaffected.
#     Verified against the data: every firm in df_merged receives a country
#     (0 missing) and the gvkey -> firm mapping is correct for country.
#     NOTE: `Country` (HQ) is used deliberately rather than Compustat `fic`
#     (country of incorporation), because fic assigns tax-haven incorporations
#     (e.g. Cayman, Bermuda) as the "country" whereas HQ is the economic location.
# =============================================================================
ref_country <- read_excel(
  glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx"),
  sheet = "master"
) %>%
  transmute(
    gvkey   = suppressWarnings(as.integer(as.character(gvkey))),
    country = Country
  ) %>%
  filter(!is.na(gvkey)) %>%
  group_by(gvkey) %>%
  slice(1) %>%
  ungroup()

df_merged <- df_merged %>%
  left_join(ref_country, by = "gvkey") %>%
  mutate(country = factor(country))

cat("Country attached: rows missing country =",
    sum(is.na(df_merged$country)),
    "| distinct countries =", n_distinct(df_merged$country), "\n")
# sanity: expect 0 missing, ~35 countries.
stopifnot(sum(is.na(df_merged$country)) == 0)


# =============================================================================
# 4. Mining subsample  (FIX 2)
#    Defined from the project NAICS2 classification, NOT from the global file.
#      primary     : NAICS 21 (Mining, Quarrying, Oil & Gas Extraction)
#      wider option: NAICS 21 + 32  (uncomment the second line, comment the first)
# =============================================================================
df_merged <- df_merged %>%
  mutate(is_mining = if_else(naics_coloc == 21, 1L, 0L, missing = 0L))
# mutate(is_mining = if_else(naics_coloc %in% c(21, 32), 1L, 0L, missing = 0L))

cat("Mining firm-years:", sum(df_merged$is_mining == 1, na.rm = TRUE),
    "| unique firms:",
    n_distinct(df_merged$gvkey[df_merged$is_mining == 1]), "\n")
# sanity: expect 560 firm-years / 63 firms, and EVERY one is NAICS 21.
stopifnot(all(df_merged$naics_coloc[df_merged$is_mining == 1] == 21))


# =============================================================================
# 5. Helpers
# =============================================================================
.dict <- c(
  fyear       = "Year FE",
  naics_coloc = "Industry FE (NAICS2)",
  country     = "Country FE (HQ)",
  colocate_index_complementary = "Complementarity Index",
  colocate_index_independent   = "Independence Index",
  colocate_index_pressure      = "Pressure Index",
  log_emp = "log(Employees)"
)

# =============================================================================
# Single output folder for EVERYTHING this script produces (LaTeX tables AND
# the merged RDS go here together). Nothing here overwrites earlier outputs.
#
# Default: a NEW subfolder inside the project results directory.
# To put the whole bundle in Dropbox instead (alongside the other data), set:
#   OUT_DIR <- glue("{DROPBOX_PATH}/cleaned_data/revised_gvkeyfix_mining21")
# Note: keeping the .tex tables under the project keeps them reachable by
# \input{} in Overleaf; a Dropbox path is not reachable from LaTeX.
# =============================================================================
OUT_DIR <- "./data/result/revised_gvkeyfix_mining21"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Filenames must NOT start with '%'. In LaTeX '%' begins a comment, so
# \input{} on a '%tab_...' file silently breaks compilation. (This was a
# documented source of earlier Overleaf errors; the leading '%' that the
# previous 07_6 scripts used on every filename has been removed.)
save_reg_table <- function(reg_model, filename) {
  etable(
    reg_model,
    coefstat = "tstat",
    dict     = .dict,
    file     = glue("{OUT_DIR}/{filename}.tex"),
    replace  = TRUE
  )
}


# =============================================================================
# 6. Regressions
#
#   Full sample : | csw0(fyear, naics_coloc, country)  -> 4 nested models
#                   col 1 no FE
#                   col 2 +year
#                   col 3 +year +industry (NAICS2)
#                   col 4 +year +industry +country (HQ)   <- the country column
#                 i.e. country is an extra column on the RIGHT of the same
#                 full-sample table, not a separate table.
#   Mining only : | csw0(fyear, country)      -> 3 nested models
#                   col 1 no FE
#                   col 2 +year
#                   col 3 +year +country (HQ)             <- the country column
#                 The NAICS2 industry FE is omitted for mining (single industry,
#                 it would absorb nothing). CAUTION: the 63 mining firms span 21
#                 countries with 15 single-firm, so the col-3 country FE behaves
#                 largely like a firm fixed effect; for Tobin's Q mining it is
#                 thinner still (5 countries, ~86% USA). Interpret col 3 with care.
#
#   Optional FE swap: to use the Compustat naicsh classification for the
#   industry FE instead of the project one, replace `naics_coloc` with `naics2`
#   in the three full-sample formulae below.
# =============================================================================

# ---- 6a. ROA --------------------------------------------------------------
reg_roa_full <- feols(
  roa ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp | csw0(fyear, naics_coloc, country),
  vcov = "HC1",
  data = df_merged
)

reg_roa_mining <- feols(
  roa ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp | csw0(fyear, country),
  vcov = "HC1",
  data = df_merged %>% filter(is_mining == 1)
)

# ---- 6b. ROS --------------------------------------------------------------
reg_ros_full <- feols(
  ros ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp | csw0(fyear, naics_coloc, country),
  vcov = "HC1",
  data = df_merged
)

reg_ros_mining <- feols(
  ros ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp | csw0(fyear, country),
  vcov = "HC1",
  data = df_merged %>% filter(is_mining == 1)
)

# ---- 6c. Tobin's Q  (US-listed firms only; tobin_q is NA for global) ------
reg_tobinq_full <- feols(
  tobin_q ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp | csw0(fyear, naics_coloc, country),
  vcov = "HC1",
  data = df_merged %>% filter(!is.na(tobin_q))
)

reg_tobinq_mining <- feols(
  tobin_q ~ colocate_index_complementary + colocate_index_independent +
    colocate_index_pressure + log_emp | csw0(fyear, country),
  vcov = "HC1",
  data = df_merged %>% filter(is_mining == 1, !is.na(tobin_q))
)


# =============================================================================
# 7. Display and save
# =============================================================================
etable(reg_roa_full,      coefstat = "tstat", dict = .dict, headers = "ROA full")
etable(reg_roa_mining,    coefstat = "tstat", dict = .dict, headers = "ROA mining")
etable(reg_ros_full,      coefstat = "tstat", dict = .dict, headers = "ROS full")
etable(reg_ros_mining,    coefstat = "tstat", dict = .dict, headers = "ROS mining")
etable(reg_tobinq_full,   coefstat = "tstat", dict = .dict, headers = "TobinQ full")
etable(reg_tobinq_mining, coefstat = "tstat", dict = .dict, headers = "TobinQ mining")

save_reg_table(reg_roa_full,      "tab_reg_roa_fullsample_logemp")
save_reg_table(reg_roa_mining,    "tab_reg_roa_mining_logemp")
save_reg_table(reg_ros_full,      "tab_reg_ros_fullsample_logemp")
save_reg_table(reg_ros_mining,    "tab_reg_ros_mining_logemp")
save_reg_table(reg_tobinq_full,   "tab_reg_tobinq_fullsample_logemp")
save_reg_table(reg_tobinq_mining, "tab_reg_tobinq_mining_logemp")


# =============================================================================
# 8. Save merged dataset into the SAME output folder as the tables
#    New folder + new filename, so the previous merged dataset
#    (regression_data_CIP_financial.RDS from the earlier 07_6 scripts) is NOT
#    overwritten. This script only READS the 09_1 output
#    (df_colocation_index_firm-year_level.RDS); it never writes to it.
# =============================================================================
df_merged %>%
  write_rds(glue(
    "{OUT_DIR}/regression_data_CIP_financial_revised_gvkeyfix_mining21.RDS"
  ))

cat("\nAll done. LaTeX tables and merged RDS saved to", OUT_DIR, "\n")