#' @title Create sdg_data for regressions
library(ggplot2)
library(fixest)
library(dplyr)
library(tidyr)

rm(list = ls()); gc()

# TODO: pick up a set of keywords to use
df_final_key <- read_rds("./data/cleaned_data/df_final_key_all.rds")
# df_final_key <- read_rds("./data/cleaned_data/df_final_key.rds")

#' @section Join two dataframes from script 1 and 2 to obtain keyword counts from all SDG categories and from all industries
# iterate through all NAICS codes
NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
sdg_data <- tibble()

for (NAICS2 in NAICS2_CODES) {
  # Load keyword counts (need to attach the corresponding SDG category label)
  df.wordCount <- read_rds(paste0("./data/cleaned_data/df_wordCount_NAICS", NAICS2, ".rds"))
  # attach SDG label and clean up firm-year observations
  df.combine <- df.wordCount %>%
    left_join(df_final_key, by = c("keyword" = "word")) %>% 
    mutate(year = as.numeric(str_extract(name, "20\\d+")),
           name = str_replace(name, "_20\\d+", ""),
           rank = as.numeric(str_extract(name, "\\d+")),
           name = str_replace(name, "\\d+\\s", ""))
  
  ## Find the numerator 
  ## the group's primary key is firm's name and SDG category
  df.long <- df.combine %>% 
    filter(n_keyword > 0) %>%
    group_by(rank, name, sdg, year) %>% 
    summarise(n_keyword = sum(n_keyword),
              rank = head(rank, 1)
    ) %>% 
    ungroup()
  
  # Plot
  df.plot <- df.long %>%
    mutate(sdg_number = as.numeric(str_extract(sdg, "\\d+"))) %>%
    mutate(sdg = as_factor(sdg)) %>%
    mutate(sdg = fct_reorder(sdg, sdg_number)) %>% 
    filter(!is.na(sdg)) %>% 
    mutate(naics = NAICS2)

  sdg_data <- bind_rows(sdg_data, df.plot)
}

# save the sdg_data
sdg_data |> write_rds("./data/cleaned_data/df_regression_by_SDG_NAICS_firmyear.RDS")


#' @section Regression

# create indicator variables
# TODO: add more indicators
data <- sdg_data %>% 
  mutate(isNAICS21 = ifelse(naics == 21, 1, 0),
         isNAICS31 = ifelse(naics == 31, 1, 0),
         isNAICS11 = ifelse(naics == 11, 1, 0)) 

# inspect column names
sdg_data %>% colnames()

# plot histograms to see the difference of distributions
sdg_data %>% 
  ggplot()+
  geom_histogram(aes(n_keyword))+
  facet_wrap(~naics, nrow = 2)

#' run regressions
#' Identification:
#'     the number of keywords mentioned in SDGXX by a company ~ industry indicators + error
# add fixed effect
reg1 <- feols(n_keyword ~ isNAICS21 | year, data = data)
# show the regression table
etable(reg1, coefstat = "tstat")

# if there is more than one industry indicators...
reg2 <- feols(n_keyword ~ isNAICS21 + isNAICS31 + isNAICS11 - 1, 
              data = sdg_data)
etable(reg2, coefstat = "tstat")
