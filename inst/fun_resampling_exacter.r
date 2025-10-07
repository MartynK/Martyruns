library(ggplot2)
library(dplyr)
library(splines)


# for (file in 
#      list.files(here::here("R"), pattern = "\\.[rR]$", full.names = TRUE)) {
#   source(file)
# }

tricubic_weight <- function(x) {
  EPSILON <- 1e-10
  if(x>0) {return(0)}
  abs_x <- abs(x/WINDOW)
  ifelse(abs_x <= 1, (1 - abs_x)^3 * (1 * abs_x^2 + 1 * abs_x + 1), EPSILON)
}
Tricubic_weight <- Vectorize(tricubic_weight)

integral_up_to_x <- function(data, x_value, 
                             kernel = "biweight", 
                             ...) {
  density_estimate <- density(data, kernel = kernel, ...)
  sum(density_estimate$y[density_estimate$x <= x_value])/
    sum(density_estimate$y)
}


source(here::here("inst","make_data_all.R"))

WINDOW   <- .05
POSITION <- 1.00
N_RESAMP <- 10000
VAR      <- "hr"

return_quants <- function(
    window = .05,
    position = 1,
    n_points = 10000,
    var = "hr",
    data_act = data_act) {
  
  data_act <- data_all %>%
    filter(data_all$start_time_scaled > POSITION - WINDOW,
           data_all$start_time_scaled <= POSITION
    )
  
  
  quants <- quantile(data_act[[var]], probs=seq(0,1,length.out=N_RESAMP)) %>%
    data.frame( var = .)
  
  return(quants)
  
}

quants <- return_quants()#var="speed")


quants %>%
  ggplot(aes(x= var)) +
  tidyquant::theme_tq()  +
  geom_histogram(aes(y=after_stat(density)),
                 alpha = 0.7, bins = 40, fill = "skyblue", color = "black") +
  geom_density(color = "red", linewidth = 1.2,
               kernel="biweight",n=2048,adjust=.6)

zones <- data.frame(
  lab   = c("Z1","LT1","Z3","Z4","Z5"),
  from  = c(120 ,153  ,160 ,178 ,189),
  until = c(153 ,160  ,178 ,189 ,210),
  ratio = NA
)

quants_proper <- quants %>% filter(var > zones$from[1],
                                   var < zones$until[nrow(zones)])

for (i in 1:nrow(zones)) {
  int_low <- which(quants$var >= zones$from[i])[1] / nrow(quants)
  int_hig <- which(quants$var >= zones$until[i])[1] / nrow(quants)
  zones$ratio[i]  <- int_hig - int_low
}

zones <- zones %>%
  mutate( ratio = ifelse(is.na(ratio)==TRUE,0,ratio))
