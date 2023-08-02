library(ggplot2)
library(dplyr)
library(splines)



for (file in 
     list.files(here::here("R"), pattern = "\\.[rR]$", full.names = TRUE)) {
  source(file)
}


source(here::here("inst","make_data_all.R"))
load(here::here("inst","training history_compiled","best_guesses.Rdata"))



pb <- txtProgressBar()
for (i in 1:nrow(best_guesses)) {
  #i <- 41
  res_coefs <- as.numeric(as.character(c(best_guesses$coef1[i],
                                         best_guesses$coef2[i],
                                         best_guesses$coef3[i]
                                         )))
  
  guessed_curve <- data.frame( y =
                                 custom_curve(x_in = seq(0,1, length.out = 1000),
                                              df = 2,
                                              custom_coefs = res_coefs))
  
  guessed_curve$x <- seq(0,12,length.out = nrow(guessed_curve))
  guessed_curve$start_time_fac <- best_guesses$start_time_fac[i]
  
  limits <- 
    data_all %>% 
    filter( as.character(start_time_fac) == best_guesses$start_time_fac[i]) %>% 
    .$speed %>%
    quantile(probs=c(0.1,0.9))
  
  guessed_curve <- guessed_curve %>% filter( x <= limits[2],
                                             x >= limits[1])
  
  if (i == 1) {
    all_guessed_curves <- guessed_curve
  } else {
    all_guessed_curves <- bind_rows( all_guessed_curves, guessed_curve)
  }
  setTxtProgressBar(pb, i/nrow(best_guesses))
}
close(pb)


all_guessed_curves <- all_guessed_curves %>%
  dplyr::rename("hr_eq" = y,
                "speed" = x) %>%
  left_join(., y = data_all %>% 
                    dplyr::select(start_time_fac,start_time_scaled) %>%
                    group_by( start_time_fac) %>%
                    slice(1),
               by = "start_time_fac")


all_guessed_curves %>%
  .[sample(1:nrow(all_guessed_curves),size = min(100000,nrow(all_guessed_curves))),] %>%
  ggplot(aes(x = speed, y = hr_eq, 
             color = start_time_scaled, 
             group = start_time_fac)) +
   theme_bw() +
   geom_line(alpha=.2)

save( all_guessed_curves, file = here::here( "inst", 
                                             "training history_compiled",
                                             "al_guessed_curves.Rdata"))
