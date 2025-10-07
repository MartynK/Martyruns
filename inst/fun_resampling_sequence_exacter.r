library(ggplot2)
library(dplyr)

return_quants <- function(
    window = .05,
    position = 1,
    n_points = 10000,
    var = "hr",
    data_all = data_all_filtered) {
  
  data_act <- data_all %>%
    filter(data_all$start_time_scaled > POSITION - WINDOW,
           data_all$start_time_scaled <= POSITION
    )
  
  
  quants <- quantile(data_act[[var]], probs=seq(0,1,length.out=N_RESAMP)) %>%
    data.frame( var = .)
  
  return(list(quants=quants,
              amount = nrow(data_act)))
  
}

source(here::here("inst","make_data_all.R"))

data_all_filtered <- data_all %>% filter(hr > 120)

load(here::here("data","d_as_in_data.rdata"))
sum_timediff <- as.numeric(e$start_time_dat[nrow(e)] - e$start_time_dat[1])



WINDOW    <- 1 / ( sum_timediff / 90) # 90 is the length of the mesocycle
NO_TIMEPOINTS <- 701
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


N_RESAMP  <- 1000


resmat <- matrix(NA, nrow = N_RESAMP, ncol = NO_TIMEPOINTS)
res_amounts <- rep(NA,NO_TIMEPOINTS)

pb <- txtProgressBar()
for ( POSITION in POSITIONS) {

  act_iter <- which(POSITION == POSITIONS)
  
  act_quant <- return_quants(window = WINDOW,
                             position = POSITION,
                             n_points = N_RESAMP,
                             data_all = data_all_filtered)
    
  resmat[,act_iter] <- act_quant$quants$var
  
  amount_in_window <- act_quant$amount
  
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

plot(colnames(as.matrix(resmat)),as.matrix(resmat)[850,],type='l')
