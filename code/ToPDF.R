install.packages("pdftools")
library(pdftools)

# Provide the correct path to the PDF file
pdf_path <- "/Users/user/Documents/GitHub/Fortune500_SDG_Analysis/data/raw_data/Fortune_500_report/1 Walmart/Walmart Annual Report 2022.pdf"

# Extract text from the PDF
ToPDF <- pdftools::pdf_text(pdf = pdf_path)

# Write the extracted text to a new file
writeLines(ToPDF, "/Users/user/Documents/GitHub/Fortune500_SDG_Analysis/data/raw_data/Fortune_500_report/1 Walmart/Walmart Annual Report 2022.txt", sep = "\t")
