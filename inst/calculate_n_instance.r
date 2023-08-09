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
  #filter( start_time_fac %in% sample(data_all$start_time_fac,size = 90)) %>%
  mutate(stf = forcats::fct_drop(start_time_fac))

table(data_all$stf)

ITER_CRITICAL <- 300
data_sessions <- unique(data_all$start_time_fac)
data_sessions <- sample(data_sessions,size=length(data_sessions))

try({
  load( file = here::here("inst",
                          "training history_compiled",
                          "best_guesses.Rdata"))
  
  data_sessions <- data_all %>% 
    group_by(start_time_fac) %>% 
    slice(1) %>% 
    left_join(.,y=best_guesses %>% 
                dplyr::select(start_time_fac, iters), 
              by="start_time_fac") %>%
    mutate( iters = iters + rnorm(n())) %>%
    rowwise %>%
    mutate( iters  = ifelse(is.na(iters)== TRUE,rnorm(1),iters)) %>%
    arrange(iters) %>%
    filter(iters < ITER_CRITICAL - 4) %>%
    .$start_time_fac
  
})

#####################

n.cores <- parallel::detectCores() - 4 # 12 logical cores - 3 for other stuff

#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores 
  ,type = "PSOCK"
)

#check cluster definition (optional)
print(my.cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered (optional)
foreach::getDoParRegistered()

foreach::getDoParWorkers()


# Define the parameter bounds
lower_bounds <- c(100,    1,   -3,    0,  0,     60,125,0)  # Lower bound for 'c'
upper_bounds <- c(300,    5,    0,  1.0, 20,     60,250,50) # Upper bound for 'c'
start_vec    <- c(168,2.719,  -.7, 0.35,  4,     60,150,0)


results <- foreach(
  i = 1:length(data_sessions),
  .verbose = TRUE,
  #.combine = 'rbind',
  .export = c("custom_curve","model", "run_bare_simulation",
              "return_the_predictions","objective",
              "data_all","lower_bounds","upper_bounds"),
  .errorhandling = 'pass',
  .inorder = FALSE,
  .packages = c("deSolve", "dplyr","splines","nloptr")
) %dopar% {
 data_local <- data_all %>% filter(start_time_fac == data_sessions[i])

 # Define the optimization problem & solving it
 # microbenchmark: ~21'' for 20 iterations for a chunk length of 1440
 optimization <- nloptr(
   x0 = start_vec,            # Initial guess for 'c'
   eval_f = objective,         # Objective function to minimize
   data_all = data_local,
   quiet = FALSE,
   lb = lower_bounds,          # Lower bounds for the parameters
   ub = upper_bounds,          # Upper bounds for the parameters
   opts = list(
     algorithm = "NLOPT_GN_MLSL",  # Optimization algorithm (e.g., COBYLA)
     maxeval = ITER_CRITICAL,             # Maximum number of function evaluations
     ftol_rel = 1e-3,            # Relative function tolerance for convergence
     print_level = 0,
     local_opts = list(algorithm = "NLOPT_LN_BOBYQA")
   )
 )
 
 res <- list(
   start_time_fac = data_sessions[i],
   optimization = optimization,
   training_history = training_history
 )
 
 # Save result to file
 saveRDS(res, file = 
           here::here("inst","training history",
           paste0(data_sessions[i],
                  "_",
                  ITER_CRITICAL,
                  ".rds")))
 
 rm(res)
 #gc()
 return(NULL)
   
}

stopImplicitCluster()
