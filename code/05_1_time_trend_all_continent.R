rm(list = ls())

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)
library(glue)
library(readxl)
source("./code/utils/func.R")
source("./code/config.R", encoding = '')

# =============================================================================
# TODO: Change NAICS2_CODE if needed
# =============================================================================
NAICS2_CODE <- 21
YEAR_MIN    <- 2015
YEAR_MAX    <- 2024

dir.create(
  glue("./data/result/TimeTrend/NAICS{NAICS2_CODE}_by_continent"),
  recursive    = TRUE,
  showWarnings = FALSE
)

# =============================================================================
# Country -> Continent mapping
# =============================================================================
continent_map <- tibble(
  country = c(
    "US", "CA", "MX",
    "BR", "CO", "CL", "PE", "AR",
    "GB", "DE", "FR", "NL", "CH", "NO", "SE", "IT", "ES", "RU", "PL",
    "CN", "JP", "KR", "IN", "SA", "SG", "TW", "HK", "MY", "ID", "TH",
    "AE", "QA", "KW", "IR", "IQ",
    "ZA", "NG", "EG", "DZ", "AO",
    "AU", "NZ"
  ),
  continent = c(
    "North America", "North America", "North America",
    "South America", "South America", "South America", "South America", "South America",
    "Europe", "Europe", "Europe", "Europe", "Europe", "Europe",
    "Europe", "Europe", "Europe", "Europe", "Europe",
    "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",
    "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",
    "Africa", "Africa", "Africa", "Africa", "Africa",
    "Oceania", "Oceania"
  )
)

# =============================================================================
# Load keyword dictionary
# =============================================================================
df_final_key <- read_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.rds")
)

# =============================================================================
# Load company-gvkey map
# =============================================================================
df.Gvkey <- getGvkeyMap(
  glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx")
)

# =============================================================================
# Load country info from master sheet
# unify gvkey as character to ensure join works correctly
# =============================================================================
df.master <- read_excel(
  glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx"),
  sheet = "master"
) %>%
  select(gvkey, country = Country) %>%
  drop_na(gvkey) %>%
  mutate(gvkey = as.character(gvkey)) %>%
  distinct(gvkey, .keep_all = TRUE)

cat(">>> Rows in df.master:", nrow(df.master), "\n")

# =============================================================================
# Load keyword count data
# =============================================================================
df <- read_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_wordCount_NAICS{NAICS2_CODE}.rds")
)

# =============================================================================
# Merge: keyword counts -> SDG label -> gvkey -> country -> continent
# unify gvkey as character across all joins to avoid type mismatch
# =============================================================================
df <- df %>%
  left_join(df_final_key, by = c("keyword" = "word")) %>%
  mutate(
    year = as.numeric(str_extract(name, "\\d{4}$")),
    name = str_remove(name, "_\\d{4}$")
  ) %>%
  left_join(
    df.Gvkey %>% mutate(gvkey = as.character(gvkey)),
    by = "name"
  ) %>%
  left_join(
    df.master %>% mutate(gvkey = as.character(gvkey)),
    by = "gvkey"
  ) %>%
  left_join(continent_map, by = "country")

# 診斷：year 範圍
cat(">>> Year range BEFORE filter:\n")
print(summary(df$year))

# 篩除異常年份
df <- df %>%
  filter(year >= YEAR_MIN, year <= YEAR_MAX) %>%
  drop_na(continent)

cat(">>> Year range AFTER filter:\n")
print(summary(df$year))
cat(">>> Countries matched to continent:\n")
print(df %>% distinct(country, continent) %>% arrange(continent, country))

# =============================================================================
# Aggregate by continent + year + SDG
# =============================================================================
df.plot <- df %>%
  group_by(continent, year, sdg) %>%
  summarise(n_keyword = sum(n_keyword), .groups = "drop") %>%
  group_by(continent, year) %>%
  mutate(n_keyword_total = sum(n_keyword)) %>%
  ungroup() %>%
  mutate(ratio = n_keyword / n_keyword_total * 100) %>%
  mutate(ratio = round(ratio, 2)) %>%
  mutate(sdg_number = as.numeric(str_extract(sdg, "\\d+"))) %>%
  mutate(sdg = as_factor(sdg)) %>%
  mutate(sdg = fct_reorder(sdg, sdg_number)) %>%
  filter(!is.na(sdg))

# =============================================================================
# Loop: one plot per continent
# =============================================================================
continents <- df.plot %>% pull(continent) %>% unique() %>% sort()
cat(">>> Continents to plot:", paste(continents, collapse = ", "), "\n")

for (cont in continents) {
  
  cat(">>> Plotting:", cont, "\n")
  df.cont <- df.plot %>% filter(continent == cont)
  
  if (nrow(df.cont) == 0) {
    cat("    No data, skipping.\n")
    next
  }
  
  MAX_YEAR <- max(df.cont$year)
  MIN_YEAR <- min(df.cont$year)
  x_breaks <- seq(MIN_YEAR, MAX_YEAR, by = 2)
  
  top_sdg <- df.cont %>%
    filter(year == MAX_YEAR) %>%
    slice_max(ratio, n = 3) %>%
    pull(sdg)
  
  y_max <- max(df.cont$ratio) + 10
  
  p <- df.cont %>%
    ggplot(aes(x = year, y = ratio, color = sdg)) +
    geom_line(linewidth = 0.8) +
    ggtitle(glue("NAICS {NAICS2_CODE} - {cont}")) +
    labs(x = "Year", y = "Percentage (%)") +
    theme_bw() +
    theme(
      axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y  = element_text(size = 12),
      plot.title   = element_text(size = 16),
      legend.title = element_text(size = 11),
      legend.text  = element_text(size = 10)
    ) +
    geom_text(
      data = df.cont %>% filter(sdg %in% top_sdg, year == MAX_YEAR),
      aes(label = sdg, y = ratio + 0.5),
      size = 3, hjust = 0, vjust = 0, check_overlap = TRUE
    ) +
    scale_x_continuous(breaks = x_breaks) +
    ylim(0, y_max)
  
  ggsave(
    plot     = p,
    filename = glue(
      "./data/result/TimeTrend/NAICS{NAICS2_CODE}_by_continent/",
      "fig_timetrend_continent_{cont}_NAICS{NAICS2_CODE}.png"
    ),
    device = "png",
    dpi    = 300,
    units  = "in",
    height = 6,
    width  = 10
  )
}

cat("\n>>> All done. Plots saved to",
    glue("./data/result/TimeTrend/NAICS{NAICS2_CODE}_by_continent/"), "\n")