# Functions repeatedly used
library(dplyr)
library(stringr)
library(assertthat)
library(tidyverse)

#----------------------------------
setwd("F:/Fortune500_SDG_Analysis")
# the above line is what we add when error come up with a message saying the path is not specify
getwd()
getRankCodeMap <- function(EXCEL_PATH = "./data/raw_data/TM Final_FortuneG500 (2021)_v2.xlsx")
  {
  "
  Description:
    the function load the excel file that records the 500 companies and the SIC code, NAICS code
    return a dataframe that shows the mapping of company and code
  "
  ## Load company Rank and NAICS code mapping
  
#  df.RankCode <- readxl::read_excel(EXCEL_PATH, 
#                                    sheet = "Fortune Global 500 2021") %>% 
#    select(rank = Rank, # 當 column name 不以數字開頭、沒特殊符號、沒有空格時，可不加back tick
#           name = Name, 
#           sic = `SIC Code`, # 當 col name 不合以上條件就要用 `` back tick 包住，是 dplyr 套件的用法
#           naics = `NAICS Code & Description (Eikon)`) %>% 
#    mutate(naics2 = floor(naics/100))
#--------------------------------------------------------------------------------------------------------------  
  df.RankCode <- readxl::read_excel("F:/Fortune500_SDG_Analysis/data/raw_data/TM Final_FortuneG500 (2021)_v2.xlsx", 
                                    sheet = "Fortune Global 500 2021") %>% 
    select(rank = Rank, # 當 column name 不以數字開頭、沒特殊符號、沒有空格時，可不加back tick
           name = Name, 
           sic = `SIC Code`, # 當 col name 不合以上條件就要用 `` back tick 包住，是 dplyr 套件的用法
           naics = `NAICS Code & Description (Eikon)`) %>% 
    mutate(naics2 = floor(naics/100)) # floor 功能是取整數，小數點後直接去掉  
  return(df.RankCode) # return function at the end 是為了確保 dataframe df.RankCode 是跑出來的結果，對Excel檔改動未雨綢繆
}
# The above is to replace row 18 to 24
#--------------------------------------------------------------------------------------------------------- 
# list the text files 
readReports <- function(NAICS2_CODE)
  {
  "
  Description:
    input: the first 2 digits of NAICS code
    output: a dataframe that selected the given codes and lists the corresponding companies and annual report contents
  "
  
  # read in files
  txt_files <- fs::dir_ls("./data/raw_data/Fortune_500_report/", 
                          # recurse means 遞歸, 會進入一層層子資料夾內取得所有檔案的路徑
                          recurse = TRUE, regexp = "\\.txt")
#  print(txt_files)
#------------------------------------------------------------------------------------------
  # make the above vector of paths as a dataframe
  df.txt <- bind_cols(
    path = txt_files) %>% 
    mutate(rank = str_replace(path, "./data/raw_data/Fortune_500_report/", "")) %>% 
    mutate(rank = str_extract(rank, pattern = "\\d+"))
  #\\d represents a digit character.
  # + is a quantifier that specifies to match one or more occurrences of the preceding pattern.
  # "\\d+" matches one or more consecutive digit characters in a string.
  
  # Select txt files based on NAICS code
  df.RankCode <- getRankCodeMap()
  
  # TO SELECT NAICS STARTING WITH THE CODE YOU INPUT
  TARGET_ROWS <- df.RankCode %>% 
  
    #cat("Which industry you want to run, NAICS number in two digits\n") %>%  
    # Prompt the user to input a value
    #input_value <- readline("Enter a value: ")
    
   # filter(naics2 == NAICS2_CODE) %>% # NAICS2_CODE要改為公司NAICS 前兩位數字，我們想看的industry
# here, you need to change the NAICS2_CODE to the two didgit that you want to run !!!!!!!!!!!!!!!!!!!!!!!
    filter(naics2 == 21) %>% # NAICS2_CODE要改為公司NAICS 前兩位數字，我們想看的industry
    pull(rank) %>% 
    unique() %>% 
    sort()
  print(TARGET_ROWS)
  
  txt.toRead <- df.txt %>% 
    filter(rank %in% TARGET_ROWS) %>% 
    pull(path)
  print(txt.toRead) #把要分析NAICS 的檔案路徑列出
  
  ## Create a new df to store the path of the file to read and the contents
  df_doc <- tibble()
  for (txt in txt.toRead) 
    {
    filename <- parseFilenameFromPath(txt)
    
    year <- str_extract(txt, "20\\d{2}")
    df.tmp <- read_lines(txt) %>% 
      as_tibble() %>% 
      summarise(value = str_c(value, collapse = "\\s")) %>% 
      mutate(name = filename,
             rank = str_extract(name, "\\d+"),
             rank = as.numeric(rank),
             year = as.numeric(year),
             name = paste0(name, "_", year)) %>% 
      drop_na() %>% 
      arrange(rank, year)
    df_doc <- df_doc %>% bind_rows(df.tmp)
  }
  
  return(df_doc)
}

parseFilenameFromPath <- function(txt_path) {
  filename <- txt_path %>% 
    str_replace("./data/raw_data/Fortune_500_report/", "") %>% 
    str_extract("\\d+.+?/") %>% 
    str_remove("/")
  return(filename)
}

testParseFilenameFromPath <- function() {
  testStrings <- c("220 Shaanxi Coal & Chemical Industry/Shaanxi Coal & Chemical Industry AR txt/Shaanxi Coal & Chemical Industry no AR.txt",
                  "191 Alimentation Couche-Tard/Shaanxi Coal & Chemical Industry AR txt/Shaanxi Coal & Chemical Industry no AR.txt",
                  "237 América Móvil/Shaanxi Coal & Chemical Industry AR txt/Shaanxi Coal & Chemical Industry no AR.txt",
                  "146 Archer Daniels Midland (ADM)/Shaanxi Coal & Chemical Industry no AR.txt",
                  "148 Xiamen C&D/Shaanxi Coal & Chemical Industry no AR.txt",
                  "328 Suning.com Group/Shaanxi Coal & Chemical Industry no AR.txt",
                  "314 J. Sainsbury/Shaanxi Coal & Chemical Industry no AR.txt",
                  "322 Itaú Unibanco Holding/Shaanxi Coal & Chemical Industry no AR.txt"
                  )
  expectedStrings <- c("220 Shaanxi Coal & Chemical Industry",
                       "191 Alimentation Couche-Tard",
                       "237 América Móvil",
                       "146 Archer Daniels Midland (ADM)",
                       "148 Xiamen C&D",
                       "328 Suning.com Group",
                       "314 J. Sainsbury",
                       "322 Itaú Unibanco Holding"
                       )
  for (i in seq_along(testStrings)) {
    cat(">>> Test case ", i, "\n")
    cat(">>>  test string is: ", testStrings[i], "\n")
    cat(">>>  expected result: ", expectedStrings[i], "\n")
    cat(">>>  parsed result is: ", parseFilenameFromPath(testStrings[i]), "\n\n")
    assertthat::assert_that(
      are_equal(expectedStrings[i],
                parseFilenameFromPath(testStrings[i]))
    )
  }
  cat(">>> All tests pass\n")
}

testParseFilenameFromPath()
