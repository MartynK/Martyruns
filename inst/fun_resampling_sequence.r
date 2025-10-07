library(ggplot2)
library(dplyr)
library(splines)

tricubic_weight <- function(x) {
  EPSILON <- 1e-10
  if(x>0) {return(0)}
  abs_x <- abs(x/WINDOW)
  ifelse(abs_x <= 1, (1 - abs_x)^3 * (1 * abs_x^2 + 1 * abs_x + 1), EPSILON)
}
Tricubic_weight <- Vectorize(tricubic_weight)

source(here::here("inst","make_data_all.R"))

data_all_filtered <- data_all %>% filter(hr > 120)


load(here::here("data","d_as_in_data.rdata"))
sum_timediff <- as.numeric(e$start_time_dat[nrow(e)] - e$start_time_dat[1])



WINDOW    <- 1 / ( sum_timediff / 90) # 90 is the length of the mesocycle
NO_TIMEPOINTS <- 201
POSITIONS <- seq(WINDOW,1.00,length.out = NO_TIMEPOINTS)

try(
  expr = {
    load(here::here("data","bootstrapped_hrs.rdata"))
    already_have <- colnames(resmat)
    POSITIONS <- POSITIONS[!(c(as.numeric(e$start_time_dat[1]) + 
                               POSITIONS * sum_timediff * 24 * 3600 ) %in% 
                               already_have)]
    
    if (length(POSITIONS)== 0) {
       POSITIONS <- runif(1)
    }
    NO_TIMEPOINTS <- length(POSITIONS)
  })


N_RESAMP  <- 10000
resmat <- matrix(NA, nrow = N_RESAMP, ncol = NO_TIMEPOINTS)
res_amounts <- rep(NA,NO_TIMEPOINTS)

pb <- txtProgressBar()
for ( POSITION in POSITIONS) {

  data_all_filtered <- data_all_filtered %>%
    mutate( difftime = data_all_filtered$start_time_scaled - POSITION,
            weight   = Tricubic_weight(difftime))

  resamp_poz <- sample(1:nrow(data_all_filtered),N_RESAMP,
                       replace = TRUE, prob=data_all_filtered$weight) %>%
    sort()
  
  resamp <- data_all_filtered$hr[resamp_poz] %>% sort
  
  act_iter <- which(POSITION == POSITIONS)
    
  resmat[,act_iter] <- resamp
  
  amount_in_window <- data_all_filtered %>%
    filter(start_time_scaled > POSITION - WINDOW,
           start_time_scaled < POSITION) %>%
    nrow()
  
  res_amounts[act_iter] <- amount_in_window
  
  setTxtProgressBar(pb, act_iter/NO_TIMEPOINTS)
}
close(pb)

# As POSIX timestamps, seconds elapsed from last epoch
colnames(resmat) <- as.numeric(e$start_time_dat[1]) + 
                      POSITIONS * sum_timediff * 24 * 3600

###

resmat_new <- resmat
res_amounts_new <- res_amounts

tryCatch(
  expr = {
    load(here::here("data","bootstrapped_hrs.rdata"))
    resmat      <- bind_cols(resmat,resmat_new)
    res_amounts <- c(res_amounts, res_amounts_new)
    new_order   <- order(colnames(resmat))
    resmat      <- resmat[,new_order]
    res_amounts <- res_amounts[new_order]
    
  },
  finally = {
    save(resmat,res_amounts, 
         file = here::here("data","bootstrapped_hrs.rdata"))
  })

plot(colnames(as.matrix(resmat)),as.matrix(resmat)[5000,],type='l')
