
# step 0: prepare coding env
source("./requirements.R")

# step 1: load and clean dictionary
source("./code/01_1_keyword.R")

# step 2: count the keyword frequencies by NAICS code
# Go to the code and specify the NAICS codes to run or it will run all sets of codes
# This is a more efficient and high-end version
source("./code/02_2_run_wordCount.R")

#' @note a simpler version without parallel computing is in the misc folder
#' Uncomment the following code to run. They're no longer supported.
# NAICS2_CODE <- 31
# source("./code/misc/02_1_countH_adjusted.R")
# NAICS2_CODE <- 33
# source("./code/misc/02_1_countH_adjusted.R")

# step 3: Create a TF-IDF Plot
# Go to the code and specify the NAICS codes to run 
source("./code/03_1_TF-IDF.R")

# step 4: Create a heatmap
# Go to the code and specify the NAICS codes to run 
# Also specify the keyword set to use in the code
source("./code/04_1_Heatmap.R")

# step 5: Create a time trend plot
# Go to the code and specify the NAICS codes to run and then pick up a company 
# Also specify the keyword set to use in the code
source("./code/05_1_time_trend.R")

# step 6: Make a table for most frequent keywords and the SDG category it belongs to
# Go to the code and specify the keyword set to use in the code
source("./code/06_1_most_frequent_keyword.R")

