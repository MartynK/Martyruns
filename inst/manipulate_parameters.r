library(dplyr)
library(ggplot2)
library(manipulate)


for (file in 
     list.files(here::here("R"), pattern = "\\.[rR]$", full.names = TRUE)) {
  source(file)
}

source(here::here("inst","make_data_all.R"))
load(here::here("inst","training history_compiled","best_guesses.Rdata"))
load(file = here::here("inst", "training history_compiled","predictions.Rdata"))

#favorite_session <- "1441727761"


data_act <- data_all %>%
  filter( start_time_fac == favorite_session)

pars <- as.numeric(as.character(best_guesses[best_guesses$start_time_fac == favorite_session,6:13]))
pars <- c(168.85,2.719,-0.7054,0.3450,1.956,60,171.2,7.6514)

manipulate({plot(1)},par1 = slider(min = 0, max = 300, initial = 291.6801083, step = 1))

# Define your manipulation function.
manipulate({
  
  pars <- c(par1, par2, par3, par4, par5, par6, par7, par8)
  
  dat_plot <- return_the_predictions(pars, data_act)
  
  cor_txt <- cor(dat_plot$hr,dat_plot$hr_eq,use= "pairwise.complete.obs")
  dev     <- sum((dat_plot$hr - dat_plot$hr_eq)^2,na.rm=TRUE)
  
  plot <- dat_plot %>%
    ggplot(aes(x = Time, y = hr)) +
    theme_bw() +
    geom_point() +
    geom_point( mapping = aes(y = hr_pred), color = "red") +
    geom_text(mapping = aes(label = 
                    round(cor(dat_plot$hr,
                        dat_plot$hr_eq,
                        use= "pairwise.complete.obs"),
                        digits = 4),
                    x = 200,
                    y = 200)) +
    geom_text(mapping = aes(label = 
                              round(sum(
                                (dat_plot$hr - dat_plot$hr_eq)^2,na.rm=TRUE)),                                digits = 4),
                            x = 200,
                            y = 170)
    
  
  print(plot)
  
  # Use sliders for the parameters that you want to manipulate.
}, 

par1 = slider(min = 0, max = 300, initial = pars[1], step = 1),
par2 = slider(min = 0, max = 50, initial = pars[2], step = 0.1),
par3 = slider(min = -1, max = 5, initial = pars[3], step = 0.01),
par4 = slider(min = 0, max = 1, initial = pars[4], step = 0.01),
par5 = slider(min = 0, max = 20, initial = pars[5], step = 0.1),
par6 = slider(min = 0, max = 100, initial = pars[6], step = 1),
par7 = slider(min = 0, max = 200, initial = pars[7], step = 1),
par8 = slider(min = 0, max = 300, initial = pars[8], step = 0.1)
)




