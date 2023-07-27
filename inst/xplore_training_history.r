library(dplyr)

tr_files <- list.files(here::here("inst",
                                  "training history"))


pb <- txtProgressBar()
for (i in 1:length(tr_files)) {
  
  filename_vars <- strsplit(tr_files[i],"_")[[1]]
  
  act_start_time_fac <- filename_vars[1]
  
  try(silent=TRUE,{
    act_chunk <- readRDS(here::here("inst", "training history", 
                                    paste0(tr_files[i]#,".rds"
                                    )))
    
    
    
  })
  
  num_rows <- nrow(act_chunk$training_history)
  out_act  <- data.frame( 
    start_time_fac = rep(act_start_time_fac,num_rows),
    iter = 1:num_rows,
    dev = act_chunk$training_history[,1],
    dev_min = rep(act_chunk$training_history[1,1],num_rows)
  )
  
  for (j in 2:num_rows) {
    out_act$dev_min[j] <- min(out_act$dev_min[j-1],
                              out_act$dev[j])
  }
  
  
  if ( i == 1) {
    out <- out_act
  } else {
    out <- bind_rows(out, out_act)
  }
  
  setTxtProgressBar(pb, i/length(tr_files))
}
close(pb)

sessions <- unique(out$start_time_fac)



out %>%
  .[sample(1:nrow(out),1000000),] %>%
  ggplot(aes(x = iter, y = log(dev_min), group = start_time_fac,
             color = start_time_fac)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_line(alpha=0.1)
