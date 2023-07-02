
# step 0: prepare coding env
source("./requirements.R")

# step 1: load and clean dictionary
source("./code/01_1_keyword.R")


# step 2:

# a simpler version
# NAICS2_CODE <- 31
# source("./code/misc/02_1_countH_adjusted.R")
# NAICS2_CODE <- 33
# source("./code/misc/02_1_countH_adjusted.R")

# a more efficient and high-end version
source("./code/02_2_run_wordCount.R")



