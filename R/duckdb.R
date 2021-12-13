# Data
json <- get_json(184328)

tipper <- get_tipper_table(json)
stat <- get_stats_table(json)
pick <- get_picks_table(json)

# Duckdb
library(DBI)
con = dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = FALSE)

## Create Tables
library(rlang)
create_tbl_query <- function(df, p_keys) {
  rlang::enexpr(df)
  query <- sprintf("CREATE TABLE %s(%s, PRIMARY KEY(%s))", rlang::enexprs(df),
                   paste(sapply(names(df), \(x) paste(x, 'VARCHAR')), collapse = ", "),
                   paste(names(df)[p_keys], collapse = ", "))
  return(query)
}

dbExecute(con, create_tbl_query(tipper, p_keys = 1))
dbExecute(con, create_tbl_query(stat, p_keys = 1:3))
dbExecute(con, create_tbl_query(pick, p_keys = 1))

## Query
### SQL Syntax
res = dbGetQuery(con, "SELECT * FROM stat WHERE win > loss")
print(res)

### Dplyr Syntax
library(dplyr)
tbl(con, "stat") %>%
  filter(win > loss)

## Export Database
dbExecute(con, "EXPORT DATABASE 'data-raw'")

dbDisconnect(con, shutdown = TRUE)
