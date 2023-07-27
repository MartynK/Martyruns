library(nloptr)
library(dplyr)
library(deSolve)
library(ggplot2)
library(doParallel)
#library(splines)



for (file in 
     list.files(here::here("R"), pattern = "\\.[rR]$", full.names = TRUE)) {
  source(file)
}

source(here::here("inst","make_data_all.R"))

data_all <- 
  data_all %>% 
    filter( start_time_fac == "1596369130")

# Define the parameter bounds
lower_bounds <- c(100,    0,  -.2,   0,  0,     60,100,50)  # Lower bound for 'c'
upper_bounds <- c(600,   20,  0.1,  20,  2,     60,210,120) # Upper bound for 'c'
start_vec    <- c(333,  1.47,-0.04, 6.8,0.2,     60,180,80)


# Define the optimization problem & solving it
# microbenchmark: ~21'' for 20 iterations for a chunk length of 1440
microbenchmark::microbenchmark({
  optimization <- nloptr(
    x0 = start_vec,                # Initial guess for 'c'
    eval_f = objective,         # Objective function to minimize
    data_all = data_all,
    quiet = FALSE,
    lb = lower_bounds,          # Lower bounds for the parameters
    ub = upper_bounds,          # Upper bounds for the parameters
    opts = list(
      algorithm = "NLOPT_GN_MLSL",  # Optimization algorithm (e.g., COBYLA)
      maxeval = 10,             # Maximum number of function evaluations
      ftol_rel = 1e-2,            # Relative function tolerance for convergence
      print_level = 3,
      local_opts = list(algorithm = "NLOPT_LN_BOBYQA")
    )
  )
}, times = 1)


# Print the optimization result
print(optimization)

# Plot the results
plot(data_all$Time, data_all$hr,ylim=c(0,220))
hrs <- return_the_predictions(
  data_all = data_all,
  x = optimization$solution#start_vec#
)
points(hrs$Time, hrs$hr_pred, col = "red")
#points(hrs$Time, hrs$hr_eq, col = "blue")

cor( hrs$hr, hrs$hr_pred, use = "pairwise.complete.obs")
cor( hrs$hr, hrs$hr_pred, use = "pairwise.complete.obs")^2

plot(res$fatigue_pred)

# # "training history" should have all the goodies
# training_history <- as.data.frame(training_history) %>%
#   .[3:nrow(training_history),]
# 
# mod <- lm(V1~.,training_history)
# summary(mod)
# plot(mod,1)
# # mod %>% effects::predictorEffects() %>% plot()
