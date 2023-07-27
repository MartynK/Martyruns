library(dplyr)

load( file = here::here("inst",
                        "training history_compiled",
                        "best_guesses.Rdata"))

# Initialize list to store results
results <- list()

# Read each result file
sessions_2_replace <- c()
for (file in list.files(here::here("inst",
                                   "training history"), 
                        full.names = TRUE)) {
  
  try(silent=TRUE,{
    act_file <- readRDS(file)
  })
  
  already_guessed <- best_guesses %>% 
    filter(start_time_fac == act_file$start_time_fac)
  
  if ( nrow(already_guessed ) > 0 &
       act_file$optimization$iterations > already_guessed$iters[1]) {
    sessions_2_replace <- c(sessions_2_replace, already_guessed$start_time_fac[1])
    results <- c(results,act_file)
  }
}

gc()

save(results, file = here::here("inst","training history_compiled",
                "results_new.Rdata"))
rm(results)

gc()

load(here::here("inst","training history_compiled",
                "results.Rdata"))

pb <-txtProgressBar()
replaced <- 0
for (i in 1:length(results2)) {
  if (results2[[(i-replaced)]]$start_time_fac %in% sessions_2_replace) {
    results2 <- results2[-(i-replaced)]        # without 2nd element
    replaced <- replaced + 1
  }
  setTxtProgressBar(pb, i/length(results2))
}
close(pb)

save(results2, file = here::here("inst","training history_compiled",
                "results.Rdata"), compress = "gzip")

gc()

load(here::here("inst","training history_compiled",
                "results_new.Rdata"))

results2 <- c(results, results2) 

unique_list_items <- data.frame( item_no = 1:length(results2),
                                 digests = "")
for (i in 1:nrow(unique_list_items)) {
  
  unique_list_items$digests[i] <- paste0( digest::digest(results2[[i]][1],"md5"),
                                          "-",
                                          digest::digest(results2[[i]][3],"md5"))
  
}
unique_list_items  <- unique_list_items %>% distinct( digests,.keep_all = TRUE)

results2 <- results2[unique_list_items$item_no]

rm(results)

gc()

save(results2, file = here::here("inst",
                                 "training history_compiled",
                                 "results.Rdata"))

# Define the directory
directory <- here::here("inst","training history")

# Get a list of all items in the directory
items <- list.files(path = directory, full.names = TRUE)

# Check which items are files (not directories)
is_file <- !file.info(items)$isdir

# Remove the files
file.remove(items[is_file])





guessed_pars <- data.frame(
  id = 1:length(results2),
  par_agn_ch = rep(NA, length(results2)),
  par_fat_1  = rep(NA, length(results2)),
  par_fat_2  = rep(NA, length(results2)),
  par_scale_agn  = rep(NA, length(results2)),
  par_scale_fat  = rep(NA, length(results2)),
  coef1 = rep(NA, length(results2)),
  coef2 = rep(NA, length(results2)),  
  coef3 = rep(NA, length(results2)),
  iters = rep(NA, length(results2)),
  start_time_fac = rep(NA, length(results2)),
  dev = rep(NA, length(results2))
)
for (i in 1:length(results2)) {
  
  guessed_pars[i,2:9] <- results2[[i]]$optimization$solution[1:8]
  guessed_pars[i,10]   <- results2[[i]]$optimization$iterations
  guessed_pars[i,11]   <- as.character(results2[[i]]$start_time_fac)
  guessed_pars[i,12]   <- results2[[i]]$optimization$objective
  
}

best_guesses <- guessed_pars %>% 
  group_by(start_time_fac) %>%
  arrange(dev) %>%
  slice(1) %>%
  .$id %>%
  guessed_pars[.,]

save( best_guesses, file = here::here("inst",
                                      "training history_compiled",
                                      "best_guesses.Rdata"))

