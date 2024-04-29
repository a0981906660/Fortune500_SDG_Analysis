library(dplyr)
library(readr)
library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='mzyxrcca')

res <- dbSendQuery(wrds, "select *
                   FROM crsp_a_ccm.comphist")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

write_csv(data, "./ccm_comphist.csv")

getwd()
