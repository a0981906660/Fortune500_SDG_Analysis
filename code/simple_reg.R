#' @title Create data for regressions
library(ggplot2)
library(fixest)
library(dplyr)
library(tidyr)

rm(list = rm()); gc()

# TODO: choose the industry and SDG category you want manually
NAICS2_CODES <- c(21, 31)
THE_SDG_CATEGORY <- "SDG13"

# change the NAICS code 
# initiate an empty data frame
data <- data_frame()
# iterate through each NAICS code (industry)
for (NAICS2 in NAICS2_CODES) {
  source("./code/04_2_psuedo_data_manipulation.R")
  df_tmp <- df.plot %>% 
    select(rank, name, n_keyword, sdg) %>% 
    filter(sdg == THE_SDG_CATEGORY) %>% 
    mutate(naics = NAICS2)
  
  data <- bind_rows(data, df_tmp)
}

# create indicator variables
# TODO: add more indicators
data <- data %>% 
  mutate(isNAICS21 = ifelse(naics == 21, 1, 0),
         isNAICS31 = ifelse(naics == 31, 1, 0)) 

# inspect column names
data %>% colnames()

# plot histograms to see the difference of distributions
data %>% 
  ggplot()+
  geom_histogram(aes(n_keyword))+
  facet_wrap(~naics, nrow = 2)

#' run regressions
#' Identification:
#'     the number of keywords mentioned in SDGXX by a company ~ industry indicators + error
reg1 <- feols(n_keyword ~ isNAICS21, data = data)
# show the regression table
etable(reg1, coefstat = "tstat")

# if there is more than one industry indicators...
reg2 <- feols(n_keyword ~ isNAICS21 + isNAICS31 - 1, 
              data = data)
etable(reg2, coefstat = "tstat")


