# install.packages("pdftools")
# install.packages("dotenv")
# install.packages("glue")

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

# company_folders <- company_folders %>% str_subset("Glencore") #如果只要一家公司，把Glencore 改成那個公司資料夾名

# iterate through each company
for (company_folder in company_folders) {
  # get the list of pdf files
  pdf_file_paths <- list.files(company_folder, full.names = F) %>%
    str_subset("\\.pdf")
  
  # get the list of txt files
  txt_file_folder <- list.files(company_folder, full.names = TRUE) %>%
    str_subset("\\.pdf", negate = TRUE)
  txt_file_folder <- txt_file_folder %>% str_subset("\\.ini", negate = TRUE)
  txt_file_paths <- list.files(txt_file_folder, full.names = F)
  
  # see which year has not been converted yet
  pdf_to_convert <- setdiff(str_remove(pdf_file_paths, "\\.pdf"), str_remove(txt_file_paths, "\\.txt"))
  
  for (a_pdf_file in pdf_to_convert) {
    
    cat(glue(">>> Processing: {a_pdf_file}\n"))
  
    # only convert the following pdf files
    pdf_to_convert_fullpath <- glue("{company_folder}/{a_pdf_file}.pdf")
    # file.exists(pdf_to_convert_fullpath[1])
    cat("\n>>> full path: ", pdf_to_convert_fullpath, "\n")
    
    # convert pdf file into tXt file
    converted_file <- pdftools::pdf_text(pdf_to_convert_fullpath)
    cat("\n>>> finish conversion\n")
    
    # save converted txt file into disk
    print(txt_file_folder)
    # print(a_pdf_file)
    filename <- glue("{txt_file_folder}/{a_pdf_file}.txt")
    cat("\n>>> filename: ", filename, "\n")
    writeLines(converted_file, 
               filename, 
               sep = "\t")
    cat(">>> finish writing")
  }
}

