
pdf_file <- "/Users/user/Documents/GitHub/Fortune500_SDG_Analysis/data/raw_data/Fortune_500_report/120 Banco Santander/Banco Santander AR 2015.pdf"
pdf_file <- "/Users/user/Documents/GitHub/Fortune500_SDG_Analysis/data/raw_data/Fortune_500_report/123 U.S. Postal Service/U.S. Postal Service AR 2015.pdf"
converted_file <- pdftools::pdf_text(pdf_file)

writeLines(converted_file, 
           "/Users/user/Documents/GitHub/Fortune500_SDG_Analysis/data/raw_data/Fortune_500_report/120 Banco Santander/Banco Santander text file/test.txt", 
           sep = "\t")

