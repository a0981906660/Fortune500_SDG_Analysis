rm(list = ls())

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)
library(glue)
source("./code/utils/func.R")
source("./code/config.R", encoding = '')

# =============================================================================
# TODO: Change NAICS2_CODE if needed
# =============================================================================
NAICS2_CODE <- 21

# 自動建立對應產業的子資料夾（若已存在則不報錯）
dir.create(
  glue("./data/result/TimeTrend/NAICS{NAICS2_CODE}"),
  recursive    = TRUE,
  showWarnings = FALSE
)

# =============================================================================
# Load data
# =============================================================================
df_final_key <- read_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.rds")
)

df.Gvkey <- getGvkeyMap(
  glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx")
)

df <- read_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS{NAICS2_CODE}_correct.RDS")
)

# =============================================================================
# Merge SDG categories and attach gvkey
# =============================================================================
df <- df %>%
  left_join(df_final_key, by = c("keyword" = "word")) %>%
  mutate(
    year = as.numeric(str_extract(name, "\\d{4}$")),
    name = str_remove(name, "_\\d{4}$")
  ) %>%
  left_join(df.Gvkey, by = "name")

# =============================================================================
# Prepare plot data
# =============================================================================
df.plot <- df %>%
  group_by(gvkey, name, year, sdg) %>%
  summarise(n_keyword = sum(n_keyword), .groups = "drop") %>%
  group_by(gvkey, name, year) %>%
  mutate(n_keyword_total = sum(n_keyword)) %>%
  ungroup() %>%
  mutate(ratio = n_keyword / n_keyword_total * 100) %>%
  mutate(ratio = round(ratio, 2)) %>%
  mutate(sdg_number = as.numeric(str_extract(sdg, "\\d+"))) %>%
  mutate(sdg = as_factor(sdg)) %>%
  mutate(sdg = fct_reorder(sdg, sdg_number)) %>%
  filter(!is.na(sdg))

# =============================================================================
# Get list of ALL companies in this NAICS
# =============================================================================
AVAILABLE_COMPANIES <- df %>%
  select(gvkey, name) %>%
  distinct() %>%
  drop_na(gvkey)

cat(">>> Total companies to plot:", nrow(AVAILABLE_COMPANIES), "\n")

# =============================================================================
# Loop through ALL companies and save one plot per company
# =============================================================================
for (i in seq_len(nrow(AVAILABLE_COMPANIES))) {
  
  COMPANY_GVKEY <- AVAILABLE_COMPANIES$gvkey[[i]]
  company_name  <- AVAILABLE_COMPANIES$name[[i]]
  
  cat(">>> Plotting", i, "/", nrow(AVAILABLE_COMPANIES), ":", company_name, "\n")
  
  # filter data for this company
  df.company <- df.plot %>% filter(gvkey == COMPANY_GVKEY)
  
  # skip if no data
  if (nrow(df.company) == 0) {
    cat("    No data found, skipping.\n")
    next
  }
  
  MAX_YEAR <- max(df.company$year)
  
  # Find top 3 SDG categories for the last year
  top_sdg <- df.company %>%
    filter(year == MAX_YEAR) %>%
    slice_max(ratio, n = 3) %>%
    pull(sdg)
  
  # scale the range of y-axis
  y_max <- max(df.company$ratio) + 10
  
  # Plot
  p <- df.company %>%
    ggplot(aes(x = year, y = ratio, color = sdg)) +
    geom_line() +
    ggtitle(company_name) +
    labs(x = "Year", y = "Percentage") +
    theme(
      axis.text.y = element_text(size = 13),
      axis.text.x = element_text(size = 13)
    ) +
    geom_text(
      data = df.company %>% filter(sdg %in% top_sdg, year == MAX_YEAR),
      aes(label = sdg, y = ratio + 0.5),
      size = 3, hjust = 0.5, vjust = 0, check_overlap = TRUE
    ) +
    scale_x_continuous(
      breaks = seq(min(df.company$year), max(df.company$year), by = 1)
    ) +
    ylim(0, y_max)
  
  # Save plot
  ggsave(
    plot     = p,
    filename = glue(
      "./data/result/TimeTrend/NAICS{NAICS2_CODE}/fig_timetrend_across_SDGcate_{company_name}.png"
    ),
    device = "png",
    dpi    = 300,
    units  = "in",
    height = 6,
    width  = 8
  )
  
}

cat("\n>>> All done. Plots saved to ./data/result/TimeTrend/NAICS", NAICS2_CODE, "/\n")