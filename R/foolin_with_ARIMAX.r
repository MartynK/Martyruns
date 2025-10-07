############
source(here::here("inst","make_data_all.R"))

library(forecast)
library(ggplot2)
library(dplyr)
library(glmnet)


#library(tseries)
#adf.test(data$hr) # is stacionary
#ccc <- ccf(data$hr,data$speed,lag=20)


data <- data_all %>%
  mutate(stt = as.numeric(start_time_fac))

######

knots <- unique(data$start_time_scaled)
knots <- knots[seq(12, length(knots), by=12)]


# Create a matrix of predictors and response
x <- model.matrix(hr_lead_18 ~ 
                  #splines2::mSpline(dist.90,degree=1) +
                  splines2::mSpline(speed, degree = 2) +
                  speed
                  * splines2::mSpline(start_time_scaled, knots = knots)
                  , data)[,-1]
y <- data$hr_lead_18

n.cores <- parallel::detectCores() - 6 # 12 logical cores - 3 for other stuff

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

gc()
Sys.sleep(1)

# Fit the LASSO model
set.seed(123)  # For reproducibility


# Cross-validation for LASSO
cvrep <- 10
folds <- ceiling( data$stt / ceiling(max(data$stt) / cvrep))
cv_lasso <- cv.glmnet(x, y, alpha=1.0, nfolds = cvrep, 
                      relax = FALSE,
                      foldid = folds,
                      parallel = TRUE,
                      trace.it = 0)


plot(cv_lasso)  # Plot the CV results

coef(cv_lasso, s=cv_lasso$lambda.min)  # Coefficients at the optimal lambda

(cv_lasso)

plot(cv_lasso$glmnet.fit,"lambda",label=FALSE)

lasso.model_direct <- glmnet(x,y,
                      alpha  = 1, 
                      lambda = cv_lasso$lambda.min)

data$hr_pred_dir <- predict(lasso.model_direct,newx=x)
data$res_dir <- data$hr_pred_dir - data$hr_lead_18

hist(data$res_dir,breaks = 90)

repmod::report(lasso.model_direct,s=cv_lasso$lambda.min)

cor(data$hr_lead_18,data$hr_pred_dir)
cor(data$hr_lead_18,data$hr_pred_dir)^2

data %>%
  filter( start_time_fac == favorite_session) %>%
  ggplot(aes(x=Time,y = hr_lead_18)) +
  theme_bw() +
  geom_line( color = "blue") +
  geom_line( mapping = aes(y = hr_pred_dir), color = "red") +
  geom_line( mapping = aes(y = speed*10), color = "green")

data %>%
  .[1:10000,] %>%
  ggplot(aes(x=hr_lead_18,y = hr_pred_dir)) +
  theme_bw() +
  geom_point( color = "blue", alpha = .1) +
  geom_abline(slope = 1, intercept = 0, color = "red")


pr <- expand.grid(
  speed = seq(4,12, length.out = 5),
  start_time_scaled = seq( 0,1,length.out = 40),
  hr_lead_18 = 0
  ) %>%
  mutate( dist.90 = speed/3.6*90)

x2 <- model.matrix(hr_lead_18 ~ 
                     #splines2::mSpline(dist.90,degree=1) +
                     splines2::mSpline(speed, degree = 2) +
                     speed
                   * splines2::mSpline(start_time_scaled, knots = knots)
                   , pr)[,-1]

pr$pr <- predict( lasso.model, newx = x2)

pr %>%
  ggplot( aes( x = start_time_scaled, y = pr, 
               color = speed, group = speed)) +
    theme_bw() +
    geom_line()

#########



knots <- unique(data$start_time_scaled)
knots <- knots[seq(60, length(knots), by=60)]


# Create a matrix of predictors and response
x <- model.matrix(hr_lead_18 ~ 
                    #splines2::mSpline(dist.90,degree=1) +
                    hr +
                    splines2::mSpline(speed, degree = 2) +
                    speed
                  * splines2::mSpline(start_time_scaled, knots = knots)
                  , data)[,-1]
y <- data$hr_lead_18

n.cores <- parallel::detectCores() - 6 # 12 logical cores - 3 for other stuff

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

gc()
Sys.sleep(1)

# Fit the LASSO model
set.seed(123)  # For reproducibility


# Cross-validation for LASSO
cvrep <- 10
folds <- ceiling( data$stt / ceiling(max(data$stt) / cvrep))
cv_lasso <- cv.glmnet(x, y, alpha=1.0, nfolds = cvrep, 
                      relax = FALSE,
                      foldid = folds,
                      parallel = TRUE,
                      trace.it = 0)


plot(cv_lasso)  # Plot the CV results

coef(cv_lasso, s=cv_lasso$lambda.min)  # Coefficients at the optimal lambda

(cv_lasso)

plot(cv_lasso$glmnet.fit,"lambda",label=FALSE)

lasso.model_comp <- glmnet(x,y,
                      alpha  = 1, 
                      lambda = cv_lasso$lambda.min)





pr <- expand.grid(
  speed = seq(2,13, length.out = 110),
  start_time_scaled = seq( 0,1,length.out = 80),
  hr_lead_18 = 0,
  hr = seq(140,220, length.out = 200)
) %>%
  mutate( dist.90 = speed/3.6*90)

x2 <- model.matrix(hr_lead_18 ~ 
                          #splines2::mSpline(dist.90,degree=1) +
                          hr +
                          splines2::mSpline(speed, degree = 2) +
                          speed
                        * splines2::mSpline(start_time_scaled, knots = knots)
                        , pr)[,-1]

pr$pr <- predict( lasso.model_comp, newx = x2)

pr <- pr %>%
  group_by(start_time_scaled, speed) %>%
  mutate( delta_hr = abs(hr - pr)) %>%
  arrange( delta_hr) %>%
  slice(1)

pr %>%
  ggplot( aes( x = start_time_scaled, y = pr, 
               color = speed, group = speed)) +
  theme_bw() +
  geom_line()


data$hr_pred_xtra <- NA

data_full <- fuzzyjoin::difference_left_join(
  data,
  pr %>% select( c(speed, pr)),
  by= "speed",
  max_dist = .2)


# data$hr_pred_xtra <- predict(lasso.model,newx=x)
# data$res <- data$hr_pred - data$hr_lead_18

hist(data$res,breaks = 90)

repmod::report(lasso.model,s=cv_lasso$lambda.min)

cor(data$hr_lead_18,data$hr_pred)
cor(data$hr_lead_18,data$hr_pred)^2
