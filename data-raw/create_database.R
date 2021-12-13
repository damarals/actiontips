# generic data for create sql schema
library(actiontips)
json <- get_json(184328)
tipper <- get_tipper_table(json)
stat <- get_stats_table(json)
tip <- get_tips_table(json)

# duckdb connect memory sql
library(DBI)
con = dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = FALSE)

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
