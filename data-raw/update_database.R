# config parallel processing
future::plan(future::multisession, workers = 6)

# update tippers
tipper <- actiontips::get_tippers_table(tippers_id = 1:100, verbose = TRUE)

# update stats
stat <- actiontips::get_stats_table(tippers_id = 1:100, verbose = TRUE)

# update tips
tip <- actiontips::get_tips_table(tippers_id = 1:100, verbose = TRUE)
