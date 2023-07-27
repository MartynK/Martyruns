

objective <- function(x, data_all = data_all, quiet = FALSE) {
  
  
  hrs <- return_the_predictions(x, data_all = data_all)
  
  # Omitting the extra calculated seconds for calculation of the deviance
  hrs_bare <-
    hrs %>% 
    filter( Time %in% data_all$Time) %>% 
    .$hr_pred
  
  # Calculating the deviance
  dev <- sum((hrs_bare - data_all$hr)^2)
  
  # parsing the results *into the parent environment!*
  if (quiet == FALSE) {
    
    if (exists("training_history") == TRUE) {
      training_history <<- rbind(training_history,
                                 c(dev,x))
    } else {
      training_history <<- c(dev,x)
    }
    
  }
  
  # returning the deviance
  return(dev)
  
}