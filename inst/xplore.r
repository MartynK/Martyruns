library(ggplot2)
library(dplyr)
library(splines)


for (file in 
     list.files(here::here("R"), pattern = "\\.[rR]$", full.names = TRUE)) {
  source(file)
}


source(here::here("inst","make_data_all.R"))


parameters <- c(
  
  par_scale = 6,  
  par_ma  = 10,  # 
  par_ar  = 25,  #
  
  list(coefs = c(60,150,30))
)

data_all <- data_all %>% 
  filter( start_time_fac == favorite_session) %>%
  mutate(hr_eq = custom_curve(x_in = speed/12, df = 2,
                              custom_coefs = c(parameters$coefs[1],
                                               parameters$coefs[2],
                                               parameters$coefs[3]
                                               )),
         resp = log(abs(hr_eq-hr)+1)*parameters$par_scale*sign(hr_eq-hr)
         )



res <- run_bare_simulation(data_all = data_all, parameters = parameters)

res %>%
  ggplot( aes( x = Time, y = hr)) +
    theme_bw() +
    geom_point() +
    geom_point(mapping = aes(y=hr_pred),color = 'red')


res2 <- return_the_predictions(
  c(0.2,0.3,0.2,60,150,30),
  data_all
  )
