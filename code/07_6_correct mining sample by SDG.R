#' @title EXPLORATORY: per-SDG ROA regressions on the CORRECTED mining sample
#' @description
#'   For each SDG (0..16) a separate regression of ROA on the three colocation
#'   indices plus log(emp) is run on the mining subsample, and all SDGs are
#'   shown side by side in one table (one column per SDG), matching the 07_8
#'   format. Three FE specifications are produced, one table each:
#'
#'       Result 1  no fixed effects
#'       Result 2  + year fixed effects
#'       Result 3  + year and country (HQ) fixed effects
#'
#'   Dependent variable : ROA
#'   Independent var    : Complementarity Index (colocate_index_complementary)
#'   Controls           : Independence Index, Pressure Index, log(emp)
#'   Standard errors    : HC1 (heteroskedasticity-robust)
#'
#'   Corrections carried over from the revised 07_6 (do NOT use the old 07_8
#'   construction, which had both of these bugs):
#'     - mining is defined as the project NAICS2 == 21, NOT membership of the
#'       global Compustat extract (that flag was all non-US-listed firms).
#'     - gvkey is cast to integer on every side before joining (the colocation
#'       file stores it as a zero-padded character, the CSVs as an integer).
#'     - HQ country is attached from the master sheet (sheet "master", column
#'       Country), deduping the one duplicate gvkey (101846, Sumitomo).
#'     - ROA / ROS / Tobin's Q are unit-free ratios, so NO currency conversion
#'       is needed; the FX block from old 07_8 is removed.
#'
#'   *** READ BEFORE INTERPRETING ***  This analysis is EXPLORATORY.
#'   Sample verified outside R against the uploaded data (63 mining firms, all
#'   correctly matched to Compustat; HQ country attaches with 0 missing):
#'     1. Per-SDG samples are NOT tiny. All 17 SDGs clear the guard
#'        (>= 10 roa obs and >= 2 firms) and are estimable in all three specs.
#'        roa obs per SDG run ~171-553 (SDG0/1/5 are the smaller ones, 40-44
#'        firms; the rest are 59-63 firms). They are smaller than a run on the
#'        old all-global "mining" (which gave ~940-1857 obs/SDG) but ample.
#'     2. 17 SDGs x 3 specs is a multiple-comparison problem: at alpha = 0.05
#'        roughly one "significant" coefficient per 20 tests is expected by
#'        chance. Lead with the Benjamini-Hochberg check in section 8, NOT the
#'        raw stars, and do not interpret any single SDG in isolation.
#'     3. Result 3 (year + country FE) estimates, but the country FE is heavy:
#'        of 19 countries, 13 have a single mining firm, so within-country
#'        identification rests mainly on US (27), Indonesia (12), China (4),
#'        Russia (3). Read its coefficients with that in mind.
#'   Known upstream gap (NOT fixed here): the source file has 11% NA gvkey from
#'   09_1's name-based join; among mining this silently drops PBF Energy
#'   (133 firm-year-SDG rows). The real fix belongs in 09_1 (join on gvkey).

# clean up environment
rm(list = ls()); gc()

library(fixest)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(glue)
source("./code/config.R")

# Console preview width (etable wraps wide tables in a narrow console;
# 17-column tables will still wrap on screen, but the saved .tex is one tabular)
options(width = 250)


# =============================================================================
# 1. Load firm-year-SDG colocation index (the PER-SDG file from 09_1)
#    Key columns: gvkey, year, sdg_number, naics (= project NAICS2),
#                 colocate_index_complementary / independent / pressure
#    FIX: cast gvkey to integer so it matches the financial side.
# =============================================================================
df_sdg <- read_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_colocation_index_firm-year-SDG_level.RDS")
) %>%
  mutate(gvkey = as.integer(as.character(gvkey)))

cat("firm-year-SDG rows:", nrow(df_sdg),
    "| firms with gvkey:", n_distinct(df_sdg$gvkey[!is.na(df_sdg$gvkey)]),
    "| SDGs present:",
    paste(sort(unique(df_sdg$sdg_number)), collapse = ","), "\n")


# =============================================================================
# 2. Financial characteristics (roa is a pre-computed ratio; no FX needed)
#    FIX: cast gvkey to integer.
# =============================================================================
sel_cols <- c("gvkey", "fyear", "conm", "emp", "roa", "ros", "tobin_q", "naics")

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
    log_emp = log(emp + 0.01)
  )


# =============================================================================
# 3. Merge -> firm-year-SDG level (one row per firm-year-SDG)
#    carry the project NAICS2 in as naics_coloc for the mining definition
# =============================================================================
df_merged <- firm_all %>%
  inner_join(
    df_sdg %>%
      select(gvkey, year, sdg_number,
             naics_coloc = naics,
             colocate_index_complementary,
             colocate_index_independent,
             colocate_index_pressure),
    by = c("gvkey" = "gvkey", "fyear" = "year")
  )

cat("Merged firm-year-SDG rows:", nrow(df_merged),
    "| unique firms:", n_distinct(df_merged$gvkey), "\n")


# =============================================================================
# 3b. Attach HQ country from the master sheet (same as the revised 07_6)
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
    sum(is.na(df_merged$country)), "\n")


# =============================================================================
# 4. Mining subsample (CORRECTED: project NAICS2 == 21)
# =============================================================================
df_mining <- df_merged %>% filter(naics_coloc == 21)

cat("\nMining firm-year-SDG rows:", nrow(df_mining),
    "| unique firms:", n_distinct(df_mining$gvkey), "\n")
stopifnot(all(df_mining$naics_coloc == 21))


# =============================================================================
# 5. Per-SDG sample sizes  - INSPECT BEFORE TRUSTING ANY COEFFICIENT
#    Any SDG with very few firm-years / firms is descriptive only.
# =============================================================================
sdg_sizes <- df_mining %>%
  group_by(sdg_number) %>%
  summarise(
    firm_years = n(),
    firms      = n_distinct(gvkey),
    n_roa      = sum(!is.na(roa)),
    countries  = n_distinct(country[!is.na(country)]),
    .groups = "drop"
  ) %>%
  arrange(sdg_number)

cat("\n================ PER-SDG SAMPLE SIZES (mining, NAICS 21) ================\n")
print(sdg_sizes, n = 30)
cat("Treat any SDG with few firms / firm-years as descriptive only.\n")
cat("For Result 3, a low `countries` count (or many 1-firm countries) means the\n")
cat("country FE is near firm FE for that SDG.\n")
cat("=========================================================================\n")


# =============================================================================
# 6. Helpers
# =============================================================================
.dict <- c(
  fyear   = "Year dummies",
  country = "Country FE (HQ)",
  colocate_index_complementary = "Complementarity Index",
  colocate_index_independent   = "Independence Index",
  colocate_index_pressure      = "Pressure Index",
  log_emp = "log(Employees)"
)

OUT_DIR <- "./data/result/revised_gvkeyfix_mining21"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Run ONE SDG under a chosen FE specification.
#   fe_part: "" (no FE) | " | fyear" (year) | " | fyear + country" (year+country)
# Guard: need at least 10 rows and 2 firms; feols wrapped in tryCatch so a
# degenerate SDG (e.g. country FE with no within variation) returns NULL.
run_one_sdg <- function(k, fe_part, data) {
  d <- data %>% filter(sdg_number == k, !is.na(roa))
  if (nrow(d) < 10 || n_distinct(d$gvkey) < 2) return(NULL)
  f <- as.formula(paste0(
    "roa ~ colocate_index_complementary + colocate_index_independent + ",
    "colocate_index_pressure + log_emp", fe_part
  ))
  tryCatch(feols(f, vcov = "HC1", data = d), error = function(e) NULL)
}

# Run every SDG for one spec, print + save the combined table (one col per SDG)
run_spec_all_sdgs <- function(fe_part, tag, data) {
  ks <- sort(unique(data$sdg_number))
  models <- map(ks, ~ run_one_sdg(.x, fe_part, data))
  names(models) <- glue("SDG{ks}")
  
  keep   <- !map_lgl(models, is.null)
  models <- models[keep]
  labels <- names(models)
  
  cat(glue("\n\n######## ROA per-SDG, mining (NAICS 21) | spec = {tag} ########\n"))
  cat("SDGs estimated:", paste(labels, collapse = ", "),
      glue(" ({length(labels)} of {length(ks)})\n"))
  print(etable(models, coefstat = "tstat", dict = .dict, headers = labels))
  
  etable(models, coefstat = "tstat", dict = .dict, headers = labels,
         file = glue("{OUT_DIR}/tab_reg_roa_mining_persdg_{tag}.tex"),
         replace = TRUE)
  
  invisible(models)
}


# =============================================================================
# 7. Run the three specifications
# =============================================================================
res_noFE        <- run_spec_all_sdgs("",                   "noFE",          df_mining)
res_yearFE      <- run_spec_all_sdgs(" | fyear",            "yearFE",        df_mining)
res_yearcountry <- run_spec_all_sdgs(" | fyear + country", "yearcountryFE", df_mining)


# =============================================================================
# 8. Multiple-comparison check (Benjamini-Hochberg) on the Complementarity
#    Index p-values across SDGs, per spec. A coefficient that is "significant"
#    raw but has p_BH > 0.05 does NOT survive correction and is not a finding.
# =============================================================================
fdr_on_complementarity <- function(models, label) {
  if (length(models) == 0) return(invisible(NULL))
  rows <- imap_dfr(models, function(m, lab) {
    ct <- tryCatch(coeftable(m), error = function(e) NULL)
    if (is.null(ct)) return(NULL)
    i <- which(rownames(ct) == "colocate_index_complementary")
    if (length(i) == 0) return(NULL)
    tibble(sdg = lab, estimate = ct[i, 1], p_raw = ct[i, 4])
  })
  if (nrow(rows) == 0) return(invisible(NULL))
  rows <- rows %>%
    mutate(p_BH = p.adjust(p_raw, method = "BH")) %>%
    arrange(p_raw)
  cat(glue("\n--- {label}: Complementarity Index across SDGs, BH-adjusted ---\n"))
  print(rows, n = 30)
  invisible(rows)
}

cat("\n\n================ MULTIPLE-COMPARISON CHECK ================\n")
cat("Benjamini-Hochberg FDR on the Complementarity Index p-values.\n")
cat("Report a coefficient as a finding only if p_BH <= 0.05.\n")
fdr_on_complementarity(res_noFE,        "ROA, no FE")
fdr_on_complementarity(res_yearFE,      "ROA, year FE")
fdr_on_complementarity(res_yearcountry, "ROA, year + country FE")
cat("==========================================================\n")


cat("\n\nDone. Three tables saved to", OUT_DIR, ":\n")
cat("  tab_reg_roa_mining_persdg_noFE.tex\n")
cat("  tab_reg_roa_mining_persdg_yearFE.tex\n")
cat("  tab_reg_roa_mining_persdg_yearcountryFE.tex\n")
cat("\nReminder: these are EXPLORATORY. Lead with the BH-adjusted check, not the\n")
cat("raw stars, and do not interpret any single SDG in isolation.\n")