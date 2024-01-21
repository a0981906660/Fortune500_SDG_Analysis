#' @title Create sdg_data for regressions
library(ggplot2)
library(fixest)
library(dplyr)
library(tidyr)

rm(list = ls()); gc()

# TODO: choose the industry and SDG category you want manually
# all industries in Excel: TM Final_FortuneG500 (2021)_v2
# NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
# 55 沒有年報 是空的

#' expected reg table:
#'         dep1. dep2. dep3. 
#' 
#' var1.     0.1. 0.2.  0.15
#' var2/    0.12  1.45. 1.6
#' ....
#' 

NAICS2_CODES <- c(11, 21, 22, 23, 31, 32, 33, 42, 44, 45, 48, 49, 51, 52, 53, 54, 60, 62, 72)
# NAICS2_CODES <- c(11, 21)
SDG_CATEGORIES <- 0:16

# testing
# NAICS2_CODES <- c(21, 31)
# SDG_CATEGORIES <- 13:15


# # 3 dependent variables based on closeness of core business
# core_sdg_category <- c(21)
# complementary_sdg_category <- c(7, 8, 9, 11, 12, 13)
# unrelated_sdg_catory <- setdiff(SDG_CATEGORIES, c(core_sdg_category, complementary_sdg_category))

# runs over each SDG category and each NAICS code
sdg_data <- data_frame()

for (THE_SDG_CATEGORY in SDG_CATEGORIES) {
  
  # for testing purpose
  THE_SDG_CATEGORY <- paste0("SDG", THE_SDG_CATEGORY)
  
  # initiate an empty sdg_data frame
  subsdg_data <- data_frame()
  # iterate through each NAICS code (industry)
  for (NAICS2 in NAICS2_CODES) {
    print(NAICS2)
    source("./code/04_2_psuedo_data_manipulation.R")
    df_tmp <- df.plot %>% 
      select(rank, name, n_keyword, sdg) %>% 
      filter(sdg == THE_SDG_CATEGORY) %>% 
      mutate(naics = NAICS2)
    
    subsdg_data <- bind_rows(subsdg_data, df_tmp)
  }
  
  # append each SGD categories
  sdg_data <- bind_rows(sdg_data, subsdg_data)
}

# save the sdg_data
sdg_data |> write_rds("./data/cleaned_data/df_regression_by_SDG_NAICS.RDS")


# create indicator variables
# TODO: add more indicators
sdg_data <- sdg_data %>% 
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
reg1 <- feols(n_keyword ~ isNAICS21, data = sdg_data)
# show the regression table
etable(reg1, coefstat = "tstat")

# if there is more than one industry indicators...
reg2 <- feols(n_keyword ~ isNAICS21 + isNAICS31 + isNAICS11 - 1, 
              data = sdg_data)
etable(reg2, coefstat = "tstat")


