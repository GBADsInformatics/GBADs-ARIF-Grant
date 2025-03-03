library(DBI)
library(config)

source('queries.R')
source('db_utils.R')

query <- get_user_info(1)
db_query(query)
