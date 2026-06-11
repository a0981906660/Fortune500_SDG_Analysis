#' @title Create sdg_data for regressions
# clean up environment
rm(list = ls()); gc()

library(ggplot2) # for plotting
library(fixest)  # for regression models
library(dplyr)   # for dataframe operations
library(tidyr)   # for data cleaning
library(stringr)
library(forcats)
library(readr)   # for RDS data loading
library(glue)    # for string concatenation
library(dotenv)  # for environmental variable loading
source("./code/utils/func.R")
source("./code/config.R", encoding = '')

# pick up a set of keywords to use
df_final_key <- read_rds(glue("{DROPBOX_PATH}/cleaned_data/df_final_key_all.rds"))
df.Gvkey     <- getGvkeyMap(
  glue("{DROPBOX_PATH}/company_reference/company_reference_master.xlsx")
)

#' @section Join two dataframes from script 1 and 2 to obtain keyword counts
#' from all SDG categories and from all industries
# iterate through all NAICS codes
NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52,
                  53, 54, 60, 62, 72)
sdg_data <- tibble()


#' @subsection Firm-year-SDG category level

# sum up firm-year-SDG level count -> firm-year-SDG level colocation index
for (NAICS2 in NAICS2_CODES) {
  # Load keyword counts (need to attach the corresponding SDG category label)
 
  #讀取舊的df_colocation_CIP_wordcount_NAICS{NAICS2}.RDS檔
  # df.wordCount <- read_rds(
  #   glue("{DROPBOX_PATH}/cleaned_data/",
  #        "df_colocation_CIP_wordcount_NAICS{NAICS2}.RDS")
  # )
  
  #只有NAICS 21 改讀
  cip_file <- if (NAICS2 == 21) {
    glue("{DROPBOX_PATH}/cleaned_data/df_colocation_CIP_wordcount_NAICS21_correct.RDS")
  } else {
    glue("{DROPBOX_PATH}/cleaned_data/df_colocation_CIP_wordcount_NAICS{NAICS2}.RDS")
  }
  df.wordCount <- read_rds(cip_file)
  
  # attach SDG label, parse year/name, and attach gvkey
  df.combine <- df.wordCount %>%
    left_join(df_final_key, by = c("keyword" = "word")) %>%
    mutate(
      year = as.numeric(str_extract(name, "\\d{4}$")),
      name = str_remove(name, "_\\d{4}$")
    ) %>%
    left_join(df.Gvkey, by = "name") %>%
    # meaningless to divide by 0, so we filter them out
    filter(n_keyword > 0)

  # compute the colocation index for each firm i, year t, SDG category k
  # denominator: total keyword occurrences for firm i, year t, SDG k
  # numerator:   keyword occurrences that co-occur with index-specific keywords
  #              within a given window length
  # ratio gives a colocation index in [0, 1] at level i,t,k

  ## primary key: firm's gvkey and SDG category
  df.long <- df.combine %>%
    group_by(gvkey, name, sdg, year) %>%
    summarise(
      n_keyword               = sum(n_keyword),
      n_complementary_keyword = sum(n_complementary),
      n_independent_keyword   = sum(n_independent),
      n_pressure_keyword      = sum(n_pressure),
      n_noCIP_keyword         = sum(n_no_cip),
      .groups = "drop"
    ) %>%
    mutate(
      colocate_index_complementary = n_complementary_keyword / n_keyword,
      colocate_index_independent   = n_independent_keyword   / n_keyword,
      colocate_index_pressure      = n_pressure_keyword      / n_keyword,
      colocate_index_noCIP         = n_noCIP_keyword         / n_keyword
    )

  # tidy up the dataframe
  df.tidy <- df.long %>%
    mutate(sdg_number = as.numeric(str_extract(sdg, "\\d+"))) %>%
    mutate(sdg = as_factor(sdg)) %>%
    mutate(sdg = fct_reorder(sdg, sdg_number)) %>%
    filter(!is.na(sdg)) %>%
    mutate(naics = NAICS2)

  sdg_data <- bind_rows(sdg_data, df.tidy)
}

# save firm-year-SDG level colocation index
sdg_data |> write_rds(
  glue("{DROPBOX_PATH}/cleaned_data/",
       "df_colocation_index_firm-year-SDG_level.RDS")
)



#' @section Firm-year level
sdg_data <- tibble()

# sum up firm-year level count -> firm-year level colocation index
for (NAICS2 in NAICS2_CODES) {
  # Load keyword counts (need to attach the corresponding SDG category label)
  
  # df.wordCount <- read_rds(
  #   glue("{DROPBOX_PATH}/cleaned_data/",
  #        "df_colocation_CIP_wordcount_NAICS{NAICS2}.RDS")
  # )
  
  cip_file <- if (NAICS2 == 21) {
    glue("{DROPBOX_PATH}/cleaned_data/df_colocation_CIP_wordcount_NAICS21_correct.RDS")
  } else {
    glue("{DROPBOX_PATH}/cleaned_data/df_colocation_CIP_wordcount_NAICS{NAICS2}.RDS")
  }
  df.wordCount <- read_rds(cip_file)
  
  # attach SDG label, parse year/name, and attach gvkey
  df.combine <- df.wordCount %>%
    left_join(df_final_key, by = c("keyword" = "word")) %>%
    mutate(
      year = as.numeric(str_extract(name, "\\d{4}$")),
      name = str_remove(name, "_\\d{4}$")
    ) %>%
    left_join(df.Gvkey, by = "name") %>%
    # meaningless to divide by 0, so we filter them out
    filter(n_keyword > 0)

  ## primary key: firm's gvkey and year
  df.long <- df.combine %>%
    group_by(gvkey, name, year) %>%
    summarise(
      n_keyword               = sum(n_keyword),
      n_complementary_keyword = sum(n_complementary),
      n_independent_keyword   = sum(n_independent),
      n_pressure_keyword      = sum(n_pressure),
      n_noCIP_keyword         = sum(n_no_cip),
      .groups = "drop"
    ) %>%
    mutate(
      colocate_index_complementary = n_complementary_keyword / n_keyword,
      colocate_index_independent   = n_independent_keyword   / n_keyword,
      colocate_index_pressure      = n_pressure_keyword      / n_keyword,
      colocate_index_noCIP         = n_noCIP_keyword         / n_keyword
    )

  # tidy up the dataframe
  df.tidy <- df.long %>%
    mutate(naics = NAICS2)

  sdg_data <- bind_rows(sdg_data, df.tidy)
}

# save firm-year level colocation index
sdg_data |> write_rds(
  glue("{DROPBOX_PATH}/cleaned_data/df_colocation_index_firm-year_level.RDS")
)

# export firm-year level colocation index as Excel for inspection
library(writexl)
sdg_data |> write_xlsx(
  glue("{DROPBOX_PATH}/cleaned_data/df_colocation_index_firm-year_level.xlsx")
)
