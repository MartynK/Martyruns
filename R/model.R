
model <- function(time, state, parameters, times_series, resps, hr_eq) {
  # Unpack state variables and parameters
  hr      <- state[1]
  prev    <- state[2]
  
  par_scale <- parameters$par_scale  
  par_ma  <- parameters$par_ma  # 
  par_ar  <- parameters$par_ar  # 


  # Import time series type of parameter
  hr_eq_function <- approxfun(times_series, hr_eq, rule = 2)
  hr_eq_act <- hr_eq_function(time)
  
  
  resp_act <- log(abs(hr_eq_act-hr)+1)*par_scale*sign(hr_eq_act-hr)
  
  # Define the system of ODEs
  hr_dt   <- resp_act * par_ma + prev * par_ar 
  prev_dt <- hr_dt - prev

  # Return the derivatives as a list
  return(list(c(hr_dt,prev_dt)))
}
