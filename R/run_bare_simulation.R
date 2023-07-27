

# Define the run_simulation function
run_bare_simulation <- function(
    data_all = data_all,
    parameters = parameters) {
  require(dplyr)
  
  data_all <- data_all %>% 
    mutate( Time = as.numeric(Time))
  
  initial_conditions <- c(hr = data_all$hr[1],
                          fatigue = 0)
  
  result <- deSolve::ode(y = initial_conditions, 
                times = data_all$Time, 
                times_series = data_all$Time, # has to be passed explicitly
                func = model, 
                parms = parameters, 
                method = "adams",
                rtol = 1e-02, # default 1e-6, but perf.increase and good enough
                atol = 1e-02, # default 1e-6, but perf.increase and good enough
                #hmin = 1e-02,
                speeds = data_all$speed,
                hr_eq = data_all$hr_eq
  ) %>%
    as.data.frame() %>%
    `colnames<-`(c("time", "hr_pred","fatigue_pred")) %>%
    select( !("time")) %>%
    bind_cols( data_all, .)
  
  return(result)
}
