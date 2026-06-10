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

# 建立輸出資料夾
dir.create(
  glue("./data/result/TimeTrend/NAICS{NAICS2_CODE}_by_country"),
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

# Load country info from master sheet
df.master <- readxl::read_excel(
  glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx"),
  sheet = "master"
) %>%
  select(gvkey, country = Country) %>%
  mutate(gvkey = as.character(sprintf("%06d", as.numeric(gvkey)))) %>%
  drop_na(gvkey) %>%
  distinct(gvkey, .keep_all = TRUE)

df <- read_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS{NAICS2_CODE}_correct.RDS")
)

# =============================================================================
# Merge SDG categories, gvkey, and country
# =============================================================================
df <- df %>%
  left_join(df_final_key, by = c("keyword" = "word")) %>%
  mutate(
    year = as.numeric(str_extract(name, "\\d{4}$")),
    name = str_remove(name, "_\\d{4}$")
  ) %>%
  left_join(df.Gvkey, by = "name") %>%
  left_join(df.master, by = "gvkey") %>%
  drop_na(country)

# =============================================================================
# Aggregate by country + year + SDG
# =============================================================================
df.plot <- df %>%
  group_by(country, year, sdg) %>%
  summarise(n_keyword = sum(n_keyword), .groups = "drop") %>%
  group_by(country, year) %>%
  mutate(n_keyword_total = sum(n_keyword)) %>%
  ungroup() %>%
  mutate(ratio = n_keyword / n_keyword_total * 100) %>%
  mutate(ratio = round(ratio, 2)) %>%
  mutate(sdg_number = as.numeric(str_extract(sdg, "\\d+"))) %>%
  mutate(sdg = as_factor(sdg)) %>%
  mutate(sdg = fct_reorder(sdg, sdg_number)) %>%
  filter(!is.na(sdg))

# =============================================================================
# Loop through each country and save one plot
# =============================================================================
countries <- df.plot %>% pull(country) %>% unique() %>% sort()
cat(">>> Countries found:", paste(countries, collapse = ", "), "\n")

for (ctry in countries) {
  
  cat(">>> Plotting country:", ctry, "\n")
  
  df.ctry <- df.plot %>% filter(country == ctry)
  
  if (nrow(df.ctry) == 0) {
    cat("    No data, skipping.\n")
    next
  }
  
  MAX_YEAR <- max(df.ctry$year)
  
  top_sdg <- df.ctry %>%
    filter(year == MAX_YEAR) %>%
    slice_max(ratio, n = 3) %>%
    pull(sdg)
  
  y_max <- max(df.ctry$ratio) + 10
  
  p <- df.ctry %>%
    ggplot(aes(x = year, y = ratio, color = sdg)) +
    geom_line() +
    ggtitle(glue("NAICS {NAICS2_CODE} - {ctry}")) +
    labs(x = "Year", y = "Percentage (%)") +
    theme(
      axis.text.y = element_text(size = 13),
      axis.text.x = element_text(size = 13),
      plot.title  = element_text(size = 16)
    ) +
    geom_text(
      data = df.ctry %>% filter(sdg %in% top_sdg, year == MAX_YEAR),
      aes(label = sdg, y = ratio + 0.5),
      size = 3, hjust = 0.5, vjust = 0, check_overlap = TRUE
    ) +
    scale_x_continuous(
      breaks = seq(min(df.ctry$year), max(df.ctry$year), by = 1)
    ) +
    ylim(0, y_max)
  
  # 檔名：fig_timetrend_country_{country code}_NAICS{code}.png
  ggsave(
    plot     = p,
    filename = glue(
      "./data/result/TimeTrend/NAICS{NAICS2_CODE}_by_country/fig_timetrend_country_{ctry}_NAICS{NAICS2_CODE}.png"
    ),
    device = "png",
    dpi    = 300,
    units  = "in",
    height = 6,
    width  = 10
  )
}

cat("\n>>> All done. Plots saved to ./data/result/TimeTrend/NAICS", NAICS2_CODE, "_by_country/\n")