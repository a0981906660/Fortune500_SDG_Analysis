# install.packages("pdftools")
library(pdftools)
library(dotenv)
library(glue)
library(stringr)
library(dplyr)

rm(list = ls())

load_dot_env()
DATA_FOLDER <- Sys.getenv("DATA_FOLDER")

# Provide the correct path to the PDF file
company_folders <- list.dirs(glue("{DATA_FOLDER}/raw_data/Fortune_500_report"), recursive = FALSE)

# company_folders <- company_folders %>% str_subset("Glencore")

# iterate through each company
for (company_folder in company_folders) {
  # get the list of pdf files
  pdf_file_paths <- list.files(company_folder, full.names = F) %>%
    str_subset("\\.pdf")
  
  # get the list of txt files
  txt_file_folder <- list.files(company_folder, full.names = TRUE) %>%
    str_subset("\\.pdf", negate = TRUE)
  txt_file_paths <- list.files(txt_file_folder, full.names = F)
  
  # see which year has not been converted yet
  pdf_to_convert <- setdiff(str_remove(pdf_file_paths, "\\.pdf"), str_remove(txt_file_paths, "\\.txt"))
  
  for (a_pdf_file in pdf_to_convert) {
    
    cat(glue(">>> Processing: {a_pdf_file}"))
  
    # only convert the following pdf files
    pdf_to_convert_fullpath <- glue("{company_folder}/{a_pdf_file}.pdf")
    # file.exists(pdf_to_convert_fullpath[1])
    
    # convert pdf file into txt file
    converted_file <- pdftools::pdf_text(pdf_to_convert_fullpath)
    
    # save converted txt file into disk
    writeLines(converted_file, 
               glue("{txt_file_folder}/{a_pdf_file}.txt"), 
               sep = "\t")
  }
}

