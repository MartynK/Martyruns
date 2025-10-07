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

WINDOW <- .05
POSITION <- 1.00


#plot(seq(-1,1,.001),Tricubic_weight(seq(-1,1,.001)))

## Softmax implementation is great at the link below but not necessary ultimately
## https://rpubs.com/FJRubio/softmax#:~:text=The%20softmax%20function%20is%20the,1exp(xj).


data_all <- data_all %>%
   mutate( difftime = data_all$start_time_scaled - POSITION,
           weight   = Tricubic_weight(difftime))

N_RESAMP <- 10000

resamp <- sample(1:nrow(data_all),N_RESAMP,
                 replace = TRUE, prob=data_all$weight) %>%
  sort()


# data_all %>%
#   .[resamp,] %>%
#   ggplot(aes(x= hr)) +
#   tidyquant::theme_tq()  +
#   geom_histogram(aes(y=after_stat(density)),
#                  alpha = 0.7, bins = 40, fill = "skyblue", color = "black") +
#   geom_density(color = "red", size = 1.2,
#                kernel="biweight",n=2048,adjust=.6)

zones <- data.frame(
  lab   = c("Z1","LT1","Z3","Z4","Z5"),
  from  = c(120 ,153  ,160 ,178 ,189),
  until = c(153 ,160  ,178 ,189 ,210),
  ratio = NA
)

for (i in 1:nrow(zones)) {
  int_low <- integral_up_to_x(data_all$hr[resamp], 
                              x_value = zones$from[i], 
                              kernel = "biweight", 
                              n = 2048, 
                              adjust = 0.6)
  
  int_hig <- integral_up_to_x(data_all$hr[resamp], 
                              x_value = zones$until[i], 
                              kernel = "biweight", 
                              n = 2048, 
                              adjust = 0.6)
  
  zones$ratio[i]  <- int_hig - int_low
}


