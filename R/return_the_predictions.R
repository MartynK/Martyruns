
# Define the function to return the predicted df.
return_the_predictions <- function(x, data_all = data_all) {
  
  # Converting time to a sane number.
  # 'hms' type is typeof()="double" so doing this every time
  data_all <- data_all %>% mutate( Time = as.numeric(Time))
  
  # Constructing the "parameters" var to be passed from the 'x' vector given
  parameters <- c(
    par_agn_ch=x[1],
    par_fat_1=x[2],
    par_fat_2=x[3],
    par_scale_agn=x[4],
    par_scale_fat=x[5],
    list(coefs = c(x[6],x[7]*(1-x[8]/100),x[7]*(x[8]/100))
    )
  )
  
  # linear interpolation for missing seconds (to be constructed)
  speed_function <- approxfun( data_all$Time, 
                               data_all$speed, rule = 2)
  
  # Constructing a df. with every second with interpolated speed and 'hr_eq'
  data_sim <- expand.grid(
    Time = seq(min(data_all$Time,na.rm=TRUE),
               max(data_all$Time,na.rm=TRUE),
               by = 1), # ADDS MISSING SECONDS
    start_time_fac = data_all$start_time_fac[1]) %>%
    mutate( speed = Time %>% speed_function(),
            id = 1:n(),
            hr_eq = custom_curve(x_in = speed/12,
                                 df = length(parameters$coef)-1,
                                 custom_coefs = parameters$coef)) %>%
    # Left joining the actual hr-s
    left_join(., data_all %>% 
                select(hr, Time, start_time_fac) %>% 
                mutate(Time = as.numeric(Time)), 
              by = c("Time","start_time_fac"))
  
  # Running the calculation step
  hrs <- run_bare_simulation(
    data_all = data_sim,
    parameters = parameters
  )
  
  return(hrs)
}