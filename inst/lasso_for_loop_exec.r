############
source(here::here("inst","make_data_all.R"))

library(forecast)
library(ggplot2)
library(dplyr)
library(glmnet)

data <- data_all %>%
  mutate(
    start_time_fac = forcats::fct_drop(start_time_fac),
    stt = as.numeric(start_time_fac),
    time = as.numeric(Time)) 

######
# 
# n        <- 4 # Number of intervals >=2
# n_lambda <- 5 # Number of points for hyperparam. tuning
# n_cross  <- 10 # number of cross validation


knots <- unique(data$start_time_scaled)
length_of_vector <- length(knots)

# Creating 'n+1' equally spaced points
indexes <- round(seq(0, length_of_vector, length.out = n + 1))
indexes <- indexes[(1:length(indexes)-1)] # chcking the last one (=n)
knots <- c(0,knots[indexes])

# Create a matrix of predictors and response

form <- as.formula(hr_lead_18 ~ 
                   time
                   + splines::ns(speed, df = 2)
                   + speed
                   * splines::bs(start_time_scaled, 
                                 #df = n + 1
                                 knots = knots
                                 ))

x <- model.matrix(form, data)[,c(-1,-5)]

cons       <- c(rep(0,3), rep(-Inf,n+3),rep(0,n+3))
cons_upper <- c(rep(Inf,3), rep(Inf,n+3),rep(5,n+3))
y          <- data$hr_lead_18

n.cores <- parallel::detectCores() - 3 # 12 logical cores - 3 for other stuff

#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores 
  ,type = "PSOCK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

foreach::getDoParWorkers()

gc()
Sys.sleep(1)

# Fit the LASSO model

# setting up a knawrly sequence

xs <- seq(0.01, pi/1.075, length.out = n_lambda)
spacing <- 1/(sin(xs) * 10 + 10) # Sine wave adjusted to have the right range - 500+ for 'stronger' sine shape
final_sequence <- cumsum(spacing) # Cumulative sum to create the sequence
final_sequence <- final_sequence - final_sequence[1] # Shift to start at 0
final_sequence <- (final_sequence / max(final_sequence)) * 12 - 10 # Scale to the range -15 to -5

# Cross-validation for LASSO
folds <- data_frame(
  stt  = 1:max(data$stt),
  fold = sample(rep(1:n_cross,ceiling(max(data$stt)/n_cross),
                    max(data$stt))))
folds <- left_join(data %>% dplyr::select(stt),folds,by="stt")

time_comp <- 
  microbenchmark::microbenchmark(times = 1, unit="seconds",
    {
      cv_lasso <- cv.glmnet(x, y, 
                            lambda = exp(final_sequence),
                            type.measure = "mae",
                            relax = TRUE,
                            foldid = folds$fold,
                            parallel = TRUE,data$stt,
                            gamma = seq(1,0,length.out=n_gamma),
                            trace.it = 0
                            ,lower.limits= cons
                            ,upper.limits= cons_upper
                            )})

data$hr_pred_dir <- predict(cv_lasso,newx=x,s="lambda.min",gamma="gamma.min")
data$res_dir <- data$hr_pred_dir - data$hr_lead_18

time_comp  <- time_comp[1,2] / 1e9
lambda_min <- cv_lasso$lambda.min
error_min  <- cv_lasso$cvm[cv_lasso$index[1,1]]
results_r2 <- cor(data$hr_lead_18,data$hr_pred_dir)^2
results_residuals_iqr <- quantile(data$res_dir,probs=c(0.25,.75))

name_of_image <- here::here("inst","training_history_lasso",
                            paste0(n,"_",n_lambda,"_",n_cross,".Rdata"))
  
save.image(file = name_of_image)

