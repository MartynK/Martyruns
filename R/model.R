
model <- function(time, state, parameters, speeds, times_series, hr_eq) {
  # Unpack state variables and parameters
  hr      <- state[1]
  fatigue <- state[2]
  
  
  par_agn_ch <- parameters$par_agn_ch
  par_fat_1  <- parameters$par_fat_1
  par_fat_2  <- parameters$par_fat_2
  par_scale_agn  <- parameters$par_scale_agn
  par_scale_fat  <- parameters$par_scale_fat
  
  
  # Import time series type of parameter
  speed_function <- approxfun(times_series, speeds, rule = 2)
  speed <- speed_function(time)
  hr_eq_function <- approxfun(times_series, hr_eq, rule = 2)
  hr_eq_act <- hr_eq_function(time)
  
  # Define the system of ODEs
  agn_change <- tanh((hr_eq_act - hr)/par_agn_ch)
  fatigue_dt <- max( tanh(fatigue+agn_change*par_fat_1),par_fat_2) - fatigue
  hr_dt      <- agn_change * par_scale_agn + fatigue_dt * par_scale_fat
  
  
  # Return the derivatives as a list
  return(list(c(hr_dt,fatigue_dt)))
}
