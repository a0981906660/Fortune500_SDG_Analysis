#' @title Regression: Mining subsample with country / region fixed effects
#' @description
#'   Mining is defined (as in 07_6 / 07_7) by the firm appearing in the
#'   project's mining gvkey list. Per the reference workbook this corresponds
#'   to gvkey_All_Mining = 130 firms (US mining 45 + Global mining 85),
#'   covering NAICS 21 AND 32. These firms span many countries and several
#'   Fortune ranking frames, so the pooled mining regression mixes very
#'   different institutional / disclosure environments. This script attaches
#'   each firm's incorporation COUNTRY and ranking SOURCE (region) from the
#'   master sheet and adds them as fixed effects, so the index coefficients
#'   are identified WITHIN country / region.
#'
#'   PRIMARY    : country fixed effect  (real institutional attribute, 0 NAs
#'                on the master sheet; recommended)
#'   ROBUSTNESS : region fixed effect   (Fortune ranking frame; 5 levels)
#'
#'   DV    : ROA, ROS, Tobin's Q (the three picks)
#'   IV    : Complementarity Index
#'   Ctrls : Independence Index, Pressure Index, log(emp) / log(at)
#'   SEs   : firm-level clustered (cluster = ~ gvkey); HC1 also printed

rm(list = ls()); gc()

library(fixest)
library(readxl)
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


# =============================================================================
# 2. Load financial characteristics (mirrors 07_7)
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


# =============================================================================
# 4. Identify mining firms (UNCHANGED from 07_6 / 07_7 - this is correct;
#    mining gvkeys are pulled separately from the North America and Global
#    Compustat extracts. Per the reference workbook this = gvkey_All_Mining
#    = 130 firms covering NAICS 21 and 32.)
# =============================================================================
gvkeys_mining <- read_csv(
  glue("{DROPBOX_PATH}/raw_data/compustat/comp_funda_global.csv")
) %>% pull(gvkey) %>% unique()

df_merged <- df_merged %>%
  mutate(is_mining = if_else(gvkey %in% gvkeys_mining, 1L, 0L))


# =============================================================================
# 5. Attach Country + Source(region) from the master sheet
#    master sheet: 587 rows, firm-level, keyed on gvkey.
#    Country has 0 NAs; Source has 0 NAs (5 ranking frames).
#    Region rule: Global 500 takes precedence over the regional lists when a
#    firm appears on more than one (Source + Second Source).
#
#    gvkey TYPE: read both sides as integer so leading zeros / float vs chr
#    mismatches cannot silently break the join.
# =============================================================================
ref <- read_excel(
  glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx"),
  sheet = "master"
)

ref_lookup <- ref %>%
  transmute(
    gvkey    = suppressWarnings(as.integer(as.character(gvkey))),
    country  = Country,
    source_1 = Source,
    source_2 = `Second Source`
  ) %>%
  filter(!is.na(gvkey)) %>%
  mutate(
    region = case_when(
      source_1 == "Fortune Global 500"         ~ "Global",
      source_2 == "Fortune Global 500"         ~ "Global",
      source_1 == "Fortune 500 (US)"           ~ "US",
      source_2 == "Fortune 500 (US)"           ~ "US",
      source_1 == "Fortune 500 Europe"         ~ "Europe",
      source_2 == "Fortune 500 Europe"         ~ "Europe",
      source_1 == "Fortune China 500"          ~ "China",
      source_2 == "Fortune China 500"          ~ "China",
      source_1 == "Fortune Southeast Asia 500" ~ "SE_Asia",
      source_2 == "Fortune Southeast Asia 500" ~ "SE_Asia",
      TRUE                                      ~ NA_character_
    )
  ) %>%
  # Sumitomo (gvkey 101846) appears twice on the master sheet; keep one row
  group_by(gvkey) %>%
  slice(1) %>%
  ungroup() %>%
  select(gvkey, country, region)

df_merged <- df_merged %>%
  mutate(gvkey = suppressWarnings(as.integer(as.character(gvkey)))) %>%
  left_join(ref_lookup, by = "gvkey") %>%
  mutate(country = factor(country),
         region  = factor(region))


# =============================================================================
# 6. Build mining sample and INSPECT before regressing.
#    Confirm: (a) how many mining firms / firm-years actually enter,
#             (b) whether the count matches the expected ~130-firm definition,
#             (c) how sparse country FE will be.
# =============================================================================
df_mining <- df_merged %>% filter(is_mining == 1)

cat("\n================ MINING SAMPLE COMPOSITION ================\n")
cat("Mining firm-years :", nrow(df_mining),
    " | unique firms :", n_distinct(df_mining$gvkey), "\n")
cat("(reference workbook expects gvkey_All_Mining = 130 firms;",
    "if unique firms is far above 130, the is_mining list is wider than\n",
    " mining and should be rechecked.)\n\n")

cat("NAICS2 present inside mining sample (expect 21 and 32):\n")
print(df_mining %>% count(naics2, sort = TRUE))

cat("\nCountry coverage (firm-years):\n")
print(df_mining %>% count(country, sort = TRUE), n = 40)

cat("\nDistinct FIRMS per country (1-firm countries => country FE ~ firm FE):\n")
print(df_mining %>% distinct(gvkey, country) %>% count(country, sort = TRUE), n = 40)

cat("\nRegion (ranking frame) coverage (firm-years):\n")
print(df_mining %>% count(region, sort = TRUE))

cat("\nMining firm-years missing COUNTRY:",
    df_mining %>% filter(is.na(country)) %>% nrow(),
    "| missing REGION:",
    df_mining %>% filter(is.na(region)) %>% nrow(), "\n")
cat("===========================================================\n")


# =============================================================================
# 7. Helpers
# =============================================================================
.reg_dict <- c(
  fyear   = "Year dummies",
  naics2  = "Industry dummies (NAICS2)",
  country = "Country FE",
  region  = "Region FE (ranking frame)",
  colocate_index_complementary = "Complementarity Index",
  colocate_index_independent   = "Independence Index",
  colocate_index_pressure      = "Pressure Index",
  log_emp = "log(Employees)",
  log_at  = "log(Total Assets)"
)
show_reg_table <- function(m) etable(m, coefstat = "tstat", dict = .reg_dict)

# -----------------------------------------------------------------------------
# Table naming convention
# -----------------------------------------------------------------------------
#   tab_reg_<dv>_<sample>_<fe>_<se>.tex
#
#     <dv>     roa | ros | tobinq
#     <sample> mining            (use 'full' if you later export full-sample)
#     <fe>     countryfe | regionfe        (the geographic FE added)
#     <se>     cl  (firm-level clustered)  | hc1 (heteroskedasticity-robust)
#
#   Plus one side-by-side table per DV/FE comparing no-geo-FE vs +geo-FE:
#     tab_reg_<dv>_<sample>_<fe>_compare.tex
#
#   IMPORTANT: filenames must NOT start with '%'. In LaTeX '%' is a comment
#   character, so \input{} on a '%tab_...' file silently breaks compilation
#   (this was a source of earlier Overleaf errors). All names below are clean.
#
#   The geographic FE is the LAST term in csw0(fyear, naics2, <geo_fe>), so it
#   is added only in column 4 of the four nested models. We therefore export
#   column 4 (the full FE specification) as the headline table for each DV.
# -----------------------------------------------------------------------------
TAB_DIR <- "./data/result/tables"

# fe label used in filenames: country -> countryfe, region -> regionfe
fe_tag <- function(geo_fe) glue("{geo_fe}fe")

# save a single fixest model (or list) to a cleanly named .tex file
save_tex <- function(m, filename) {
  etable(m, coefstat = "tstat", dict = .reg_dict,
         file = glue("{TAB_DIR}/{filename}.tex"), replace = TRUE)
}

# Estimate one DV under a chosen geo FE; both HC1 and clustered.
# csw0(fyear, naics2, geo_fe) -> 4 nested models; col 4 = full FE set
# (year + NAICS2 + country/region). Compare col 3 vs col 4 to see what the
# geographic FE adds.
run_pick <- function(dep, controls, data, geo_fe, tag) {
  f <- as.formula(glue(
    "{dep} ~ colocate_index_complementary + colocate_index_independent +",
    " colocate_index_pressure + {controls} | csw0(fyear, naics2, {geo_fe})"
  ))
  m_hc1 <- feols(f, vcov = "HC1",      data = data)
  m_cl  <- feols(f, cluster = ~ gvkey, data = data)
  
  cat(glue("\n==== {tag} | geoFE={geo_fe} | HC1 ====\n"));       print(show_reg_table(m_hc1))
  cat(glue("\n==== {tag} | geoFE={geo_fe} | clustered ====\n")); print(show_reg_table(m_cl))
  
  ft <- fe_tag(geo_fe)
  
  # 1) headline full-FE column (col 4), clustered  -- the table for the thesis
  save_tex(m_cl[[4]],  glue("tab_reg_{tag}_mining_{ft}_cl"))
  # 2) same column under HC1                       -- transparency / appendix
  save_tex(m_hc1[[4]], glue("tab_reg_{tag}_mining_{ft}_hc1"))
  # 3) side-by-side: no-geo-FE (col 3) vs +geo-FE (col 4), clustered
  cmp <- etable(m_cl[[3]], m_cl[[4]], coefstat = "tstat", dict = .reg_dict,
                headers = c("Year+NAICS2", glue("+{geo_fe} FE")))
  print(cmp)
  etable(m_cl[[3]], m_cl[[4]], coefstat = "tstat", dict = .reg_dict,
         headers = c("Year+NAICS2", glue("+{geo_fe} FE")),
         file = glue("{TAB_DIR}/tab_reg_{tag}_mining_{ft}_compare.tex"),
         replace = TRUE)
  
  invisible(list(hc1 = m_hc1, cl = m_cl))
}


# =============================================================================
# 8. PRIMARY: country fixed effect
# =============================================================================
cat("\n\n############ PRIMARY: COUNTRY FE ############\n")
roa_country    <- run_pick("roa",     "log_emp", df_mining,                              "country", "roa")
ros_country    <- run_pick("ros",     "log_at",  df_mining,                              "country", "ros")
tobinq_country <- run_pick("tobin_q", "log_emp", df_mining %>% filter(!is.na(tobin_q)),  "country", "tobinq")


# =============================================================================
# 9. ROBUSTNESS: region (ranking-frame) fixed effect
# =============================================================================
cat("\n\n############ ROBUSTNESS: REGION FE ############\n")
roa_region    <- run_pick("roa",     "log_emp", df_mining,                              "region", "roa")
ros_region    <- run_pick("ros",     "log_at",  df_mining,                              "region", "ros")
tobinq_region <- run_pick("tobin_q", "log_emp", df_mining %>% filter(!is.na(tobin_q)),  "region", "tobinq")


cat("\n\nDone. Read Section 6 FIRST: confirm unique mining firms is near 130 and\n")
cat("check how many countries have only 1 firm. Then compare the\n")
cat("'Year+NAICS2' vs '+geo FE' columns for each DV: if the Pressure Index\n")
cat("stays negative and significant after country FE, the relationship holds\n")
cat("within country; if it weakens, the earlier estimate was partly driven by\n")
cat("cross-country composition rather than a within-country effect.\n")

cat("\nTables written to", TAB_DIR, ":\n")
cat("  Country FE (primary):\n")
cat("    tab_reg_{roa,ros,tobinq}_mining_countryfe_cl.tex       (clustered, headline)\n")
cat("    tab_reg_{roa,ros,tobinq}_mining_countryfe_hc1.tex      (HC1, transparency)\n")
cat("    tab_reg_{roa,ros,tobinq}_mining_countryfe_compare.tex  (no-FE vs +country FE)\n")
cat("  Region FE (robustness):\n")
cat("    tab_reg_{roa,ros,tobinq}_mining_regionfe_cl.tex\n")
cat("    tab_reg_{roa,ros,tobinq}_mining_regionfe_hc1.tex\n")
cat("    tab_reg_{roa,ros,tobinq}_mining_regionfe_compare.tex\n")
cat("\nInput in LaTeX with e.g. \\input{tables/tab_reg_roa_mining_countryfe_cl}\n")