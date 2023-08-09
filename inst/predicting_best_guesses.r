library(ggplot2)
library(dplyr)

for (file in 
     list.files(here::here("R"), pattern = "\\.[rR]$", full.names = TRUE)) {
  source(file)
}


source(here::here("inst","make_data_all.R"))
load(here::here("inst","training history_compiled","best_guesses.Rdata"))

#best_guesses <- best_guesses %>% filter(iters == 1000)

# GGally::ggpairs(best_guesses[,5:15]) +
#   theme_bw()


pb <- txtProgressBar()
for (i in 1:nrow(best_guesses)) {
  data_act <- data_all %>% 
    filter(as.character(start_time_fac) == best_guesses$start_time_fac[i]) %>%
    mutate(Time = as.numeric(Time))
  
  restable <- return_the_predictions(
    data_all = data_act,
    x = as.numeric(as.character(best_guesses[i,6:13]))
  )
  
  data_act <- left_join(data_act, 
                        restable %>% 
                          dplyr::select(c(Time, hr_pred,fatigue_pred)),
                        by="Time")
  
  if (i == 1) {
    data_out <- data_act
  } else {
    data_out <- bind_rows( data_out, data_act)
  }
  setTxtProgressBar(pb, i/nrow(best_guesses))
}
close(pb)

downsamp <- sample(1:nrow(data_out),size = min(200000,nrow(data_out)))

data_out[downsamp,] %>%
  ggplot( aes(x=hr,y=hr_pred
              #,group=start_time_fac
              )) +
    theme_bw() +
    geom_point(alpha=0.1,size=.5) +
    geom_smooth(color="green") +
    geom_abline(slope=1,intercept=0,color="red")

data_predicted <- data_out

save( data_predicted, file = here::here("inst",
                                        "training history_compiled",
                                        "predictions.Rdata"))

data_predicted %>%
  filter( start_time_fac %in%
            c(
            "1453037625",
            "1595917789",
            "1652122732",
            "1660124079",
            "1357504789"
          )) %>%
  ggplot(aes(x=Time,y=hr)) +
    theme_bw() +
    geom_point() +
    geom_point(mapping=aes(y=hr_pred),color = "red") +
    facet_wrap(facets="start_time_scaled",scales="free_x")

cor(data_out$hr,data_out$hr_pred, use = "pairwise.complete.obs")
cor(data_out$hr,data_out$hr_pred, use = "pairwise.complete.obs")^2


