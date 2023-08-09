library(ggplot2)
library(dplyr)
library(splines)

# Define the tricube weighting function
tricube_mod <- function(x) {
  weight <- (1 - abs(x)^3)^3
  ifelse(weight < 0, 0, weight)
}
Tricube_mod <- Vectorize(tricube_mod)

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

SCALE_WINDOW     <- 0.15 # this period is treated as a 'distance' of 1, weight decays to 10%

times_of_interest <- seq(0,1,length.out = 11)

for (i in 1:length(times_of_interest)) {

  all_guessed_curves <- all_guessed_curves %>%
    mutate( weight = Tricube_mod(abs(times_of_interest[i] - 
                                       start_time_scaled)/SCALE_WINDOW))
  
  if (sum(all_guessed_curves$weight)> 0) {
    mod <- lm( hr_eq ~ splines2::mSpline(speed, 
                                         df = 3 
                                         ,Boundary.knots=c(4,12)
                                         #,degree = 3, 
                                         #intercept = TRUE
                                         ), 
               data = all_guessed_curves, 
               weights = all_guessed_curves$weight)
    
    pr <- expand.grid( speed = seq(0,12,length.out = 1000),
                       start_time_scaled = times_of_interest[i])
    pr$pr <- predict(mod, newdata = pr)
    
    evaled_speeds <- all_guessed_curves %>% 
      filter( weight > 0.1) %>% 
      .$speed %>% summary() %>% 
      .[c(1,6)]
    if( is.na(evaled_speeds[1]) == TRUE) {
      evaled_speeds <- c(0,0)
    }
    
    pr$speed_recorded <- sapply(1:nrow(pr), function(x){
      if(pr$speed[x] >= evaled_speeds[1] &  pr$speed[x] <= evaled_speeds[2]){
        TRUE
      } else {
        FALSE
      }
    })
    
    if ( i == 1) {
      pr_out <- pr
    } else {
      pr_out <- bind_rows( pr_out, pr)
    }

  }

}

pr_out %>%
  filter(speed_recorded == TRUE) %>%
  ggplot( aes( x = speed, y = pr, color = start_time_scaled,
               group = start_time_scaled)) +
    theme_bw() +
    geom_line(alpha = 1,linewidth = .75) +
    scale_x_continuous(breaks = c(4,5,6,7,8,9,10,12)) +
    scale_y_continuous(breaks = c(100,110,120,130,140,150,160,170,180,190,200))


  