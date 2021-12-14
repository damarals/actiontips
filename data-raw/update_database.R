# get data
## config parallel processing
future::plan(future::multisession, workers = 6)

## update tippers
tipper <- actiontips::get_tippers_table(tippers_id = 1:100, verbose = TRUE)

## update stats
stat <- actiontips::get_stats_table(tippers_id = 1:100, verbose = TRUE)

## update tips
tip <- actiontips::get_tips_table(tippers_id = 1:100, verbose = TRUE)

# duckdb connect memory sql
library(DBI)
con = dbConnect(drv = duckdb::duckdb(), dbdir = "data-raw/database.duckdb")

# write in database
dbWriteTable(con, "tipper", tipper, append = TRUE)
dbWriteTable(con, "stat", stat, append = TRUE)
dbWriteTable(con, "tip", tip, append = TRUE)

# close connection
dbDisconnect(con, shutdown = TRUE)
