# generic data for create sql schema
library(actiontips)
tipper <- get_tippers_table(1)
stat <- get_stats_table(1)
tip <- get_tips_table(1)

# duckdb connect memory sql
library(DBI)
con = dbConnect(drv = duckdb::duckdb(), dbdir = "data-raw/database.duckdb")

## create tables
library(rlang)
create_tbl_query <- function(df, col_types, p_keys) {
  sprintf("CREATE TABLE %s(%s, PRIMARY KEY(%s))", enexprs(df),
          paste(paste(names(df), col_types), collapse = ", "),
          paste(names(df)[p_keys], collapse = ", "))
}

coltp_tipper <- c("INTEGER", "VARCHAR", "LOGICAL", "LOGICAL",
                  "LOGICAL", "INTEGER")
coltp_stat <- c("INTEGER", "VARCHAR", "VARCHAR", "INTEGER",
                "INTEGER", "INTEGER")
coltp_tip <- c("INTEGER", "INTEGER", "INTEGER", "DATETIME",
               "DATETIME", "VARCHAR", "VARCHAR", "VARCHAR")

dbExecute(con, create_tbl_query(tipper, col_types = coltp_tipper, p_keys = 1))
dbExecute(con, create_tbl_query(stat, col_types = coltp_stat, p_keys = 1:3))
dbExecute(con, create_tbl_query(tip, col_types = coltp_tip, p_keys = 1))

## close connection
dbDisconnect(con, shutdown = TRUE)
