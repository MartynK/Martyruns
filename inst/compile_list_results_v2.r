library(dplyr)
library(ggplot2)

tr_files <- list.files(here::here("inst",
                                  "training history"))

guessed_pars <- data.frame(
  id = 1:length(tr_files),
  start_time_fac = rep(NA, length(tr_files)),
  iters = rep(NA, length(tr_files)),
  duration = rep(NA, length(tr_files)),
  dev = rep(NA, length(tr_files)),
  
  par_agn_ch = rep(NA, length(tr_files)),
  par_fat_1  = rep(NA, length(tr_files)),
  par_fat_2  = rep(NA, length(tr_files)),
  par_scale_agn  = rep(NA, length(tr_files)),
  par_scale_fat  = rep(NA, length(tr_files)),
  coef1 = rep(NA, length(tr_files)),
  coef2 = rep(NA, length(tr_files)),  
  coef3 = rep(NA, length(tr_files))

)


pb <- txtProgressBar()
for (i in 1:length(tr_files)) {
  
  filename_vars <- strsplit(tools::file_path_sans_ext(tr_files[i]),"_")[[1]]
  
  guessed_pars$start_time_fac[i] <- filename_vars[1]
  guessed_pars$iters[i] <- as.numeric(filename_vars[2])
  
  
  try(silent=TRUE,{
    act_chunk <- readRDS(here::here("inst", "training history", 
                                    paste0(tr_files[i]#,".rds"
                                           )))
       
    guessed_pars$dev[i]        <- act_chunk$optimization$objective
    
    guessed_pars$par_agn_ch[i]    <- act_chunk$optimization$solution[1]
    guessed_pars$par_fat_1[i]     <- act_chunk$optimization$solution[2]
    guessed_pars$par_fat_2[i]     <- act_chunk$optimization$solution[3]
    guessed_pars$par_scale_agn[i] <- act_chunk$optimization$solution[4]
    guessed_pars$par_scale_fat[i] <- act_chunk$optimization$solution[5]
    guessed_pars$coef1[i] <- act_chunk$optimization$solution[6]
    guessed_pars$coef2[i] <- act_chunk$optimization$solution[7]    
    guessed_pars$coef3[i] <- act_chunk$optimization$solution[8]

  })
  
  setTxtProgressBar(pb, i/length(tr_files))
}
close(pb)

source(here::here("inst","make_data_all.R"))

for (i in 1:nrow(guessed_pars)) {
  duration <- data_all %>% 
    filter(start_time_fac == guessed_pars$start_time_fac[i]) %>% 
    mutate( Time = as.numeric(Time)) %>% 
    .$Time %>% 
    summary()
  duration <- duration[6] - duration[1]
  guessed_pars$duration[i] <- duration
}

guessed_pars <- guessed_pars %>% 
  mutate( iters = as.numeric(iters),
          #start_time_fac = forcats::(start_time_fac)
          )


guessed_pars$dev_per_duration <- guessed_pars$dev / guessed_pars$duration
guessed_pars <- left_join( guessed_pars,
  y = data_all %>% 
    dplyr::select(c(start_time_fac,
                    start_time_scaled)) %>% 
    group_by(start_time_fac) %>% 
    slice(1))

best_guesses <- guessed_pars %>% 
  group_by(start_time_fac) %>%
  arrange(dev) %>%
  slice(1) %>%
  .$id %>%
  guessed_pars[.,]

save( best_guesses, file = here::here("inst",
                                      "training history_compiled",
                                      "best_guesses.Rdata"))





#GGally::ggpairs(best_guesses[,c(3,6:15)]) +
#   theme_bw()