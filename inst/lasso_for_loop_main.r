library(tcltk)
library(dplyr)

# Initialize variable
buttonClicked <- FALSE

# Function to change buttonClicked to TRUE
onClick <- function(event) {
  assign("buttonClicked", TRUE, envir = .GlobalEnv)
  print("Canvas clicked!")
}

# Create main window
win <- tktoplevel()

# Create a canvas widget
canvas <- tkcanvas(win, width = 200, height = 200)
tkpack(canvas)

# Bind a left mouse button click on the canvas to the onClick function
tkbind(canvas, "<Button-1>", onClick)

while(TRUE) {
  
  param_table <- data.frame(
    n        = rbinom(1,120,runif(1,.45,.85))+1, # Number of intervals >=2
    n_lambda = (rbinom(1,40,.25)*2 + 1), # Number of points for hyperparam. tuning
    n_cross  = (rbinom(1,60,.55) + 1), # number of cross validation
    n_gamma  = rbinom(1,20,.3) + 1
  )
  
  print(param_table)
  
  n        <- param_table$n[1]
  n_lambda <- param_table$n_lambda[1]
  n_cross  <- param_table$n_cross[1]
  n_gamma  <- param_table$n_gamma[1]
  
  source( here::here("inst","lasso_for_loop_exec.r"))
  
  
  results_df <- data.frame(
    time_elaps = time_comp,
    lambda_min = lambda_min,
    error_min = error_min,
    r2 = results_r2,
    res_iqr_1 = results_residuals_iqr[1],
    res_iqr_2 = results_residuals_iqr[2]
  )
  
  results_df_comp <- bind_cols(param_table,results_df)
  
  complete_training_file <- here::here("inst","training_history_lasso_compiled.Rdata")
  first_iter <- !(file.exists(complete_training_file))
  
  if (first_iter == TRUE) {
    results_df_comp_long <- results_df_comp
  } else {
    load(complete_training_file)
    results_df_comp_long <- bind_rows(results_df_comp_long,
                                      results_df_comp)
  }
  save( results_df_comp_long, file = complete_training_file)
  
  # Check the status of buttonClicked
  if(buttonClicked) {
    print("The canvas was clicked.")
    break()
  }
  
}  

# Destroy the window
tkdestroy(win)
