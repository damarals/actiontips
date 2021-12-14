# generic data for create sql schema
library(actiontips)
tipper <- get_tippers_table(0)
stat <- get_stats_table(0)
tip <- get_tips_table(0)

# duckdb connect memory sql
library(DBI)
con = dbConnect(drv = duckdb::duckdb(), dbdir = "data-raw/database.duckdb")

## create tables
library(rlang)
create_tbl_query <- function(df, p_keys) {
  sprintf("CREATE TABLE %s(%s, PRIMARY KEY(%s))", enexprs(df),
          paste(paste(names(df), 'VARCHAR'), collapse = ", "),
          paste(names(df)[p_keys], collapse = ", "))
}

dbExecute(con, create_tbl_query(tipper, p_keys = 1))
dbExecute(con, create_tbl_query(stat, p_keys = 1:3))
dbExecute(con, create_tbl_query(tip, p_keys = 1))

## export database
dbExecute(con, "EXPORT DATABASE 'data-raw'")

## close connection
dbDisconnect(con, shutdown = TRUE)
