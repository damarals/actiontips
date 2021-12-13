# config parallel processing
future::plan(future::multisession, workers = 15)

# update tippers
tictoc::tic()
tipper <- actiontips::get_tippers_table(tippers_id = 1:100000, verbose = TRUE)
tictoc::toc()

Sys.sleep(60)

# update stats
tictoc::tic()
stat <- actiontips::get_stats_table(tippers_id = 1:100000, verbose = TRUE)
tictoc::toc()

Sys.sleep(60)

# update tips
tictoc::tic()
tip <- actiontips::get_tips_table(tippers_id = 1:100000, verbose = TRUE)
tictoc::toc()
