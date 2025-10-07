############
source(here::here("inst","make_data_all.R"))

library(forecast)
library(ggplot2)
library(dplyr)
library(glmnet)

#library(tseries)
#adf.test(data$hr) # is stacionary
#ccc <- ccf(data$hr,data$speed,lag=20)

set.seed(123)
data <- data_all %>%
  filter( start_time_fac %in% 
            unique(start_time_fac)[
              #sample(1:(length(unique(data_all$start_time_fac))),300)
              ])

data <- data %>%
  mutate(
    start_time_fac = forcats::fct_drop(start_time_fac),
    stt = as.numeric(start_time_fac),
    time = as.numeric(Time)) 


length(unique(data$stt))
max(data$stt)

######

dff <- 50
n <- 120 # Number of intervals >=2

knots <- unique(data$start_time_scaled)

length_of_vector <- length(knots)

# Creating 'n+1' equally spaced points
indexes <- round(seq(0, length_of_vector, length.out = n + 1))
indexes <- indexes[(1:length(indexes)-1)] # chcking the last one (=n)
knots <- knots[indexes]

# Create a matrix of predictors and response

form <- as.formula(hr_lead_18 ~ 
                     time
                   #+ splines2::iSpline(speed, degree = 1)[,1]
                   + splines2::iSpline(speed, degree = 1)
                   + speed 
                   * splines::bs(start_time_scaled, 
                                 #df = dff
                                 knots = knots
                                 ))



x <- model.matrix(form, data)[,c(-1,-5)]

cons <- c(rep(-Inf,3), rep(-Inf,n+2),rep(0,n+2))
y <- data$hr_lead_18

n.cores <- parallel::detectCores() - 9 # 12 logical cores - 3 for other stuff

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

# setting up a knawrly sequence
n <- 40 # Number of points
xs <- seq(0.01, pi/1.075, length.out = n)
spacing <- 1/(sin(xs) * 150 + 10) # Sine wave adjusted to have the right range
final_sequence <- cumsum(spacing) # Cumulative sum to create the sequence
final_sequence <- final_sequence - final_sequence[1] # Shift to start at 0
final_sequence <- (final_sequence / max(final_sequence)) * 6 - 4 # Scale to the range -15 to -5

# Print the result
plot(final_sequence)

# Cross-validation for LASSO
n_cross <- max(data$stt)
folds <- data$stt#ceiling( data$stt / ceiling(max(data$stt) / cvrep))
folds <- data.frame(
stt = 1:max(data$stt)
)
folds_bound <- round(seq(1,max(data$stt),length.out = n_cross))-1
folds <- folds %>%
  rowwise() %>%
  mutate(fold = max(which(folds_bound < stt)))
folds <- left_join(data %>% dplyr::select(stt),folds,by="stt")

cv_lasso <- cv.glmnet(x, y, 
                      lambda = exp(final_sequence),
                      #lambda = exp(seq(-15,-5,length.out=15)),
                      type.measure = "mae",
                      relax = TRUE,
                      foldid = folds$fold,
                      parallel = TRUE,data$stt,
                      gamma = seq(1,0,length.out=3),
                      trace.it = 0
                      ,lower.limits= cons
                      )

plot(cv_lasso, se.bands = FALSE)  # Plot the CV results

for (i in 1:length(cv_lasso$relaxed$statlist)) {
  d_chunk <- 
    data.frame(
      l = cv_lasso$relaxed$statlist[[i]]$lambda,
      y = cv_lasso$relaxed$statlist[[i]]$cvm,
      g = rep( as.numeric(gsub("g:", "", names(cv_lasso$relaxed$statlist)))[i],
               length(cv_lasso$relaxed$statlist[[i]]$lambda))
    )
  
  if (i == 1) {
    out <- d_chunk
  } else {
    out <- bind_rows( out, d_chunk)
  }
}

out %>%
  ggplot( aes(x = log(l), y = y, 
              color = g, group = g)) +
    theme_bw() +
    geom_point() +
    geom_line() +
    scale_y_continuous( limits = quantile(out$y)[c(1,2)])


coef(cv_lasso$glmnet.fit, s=cv_lasso$lambda.min,
     gamma = cv_lasso$relaxed$gamma.min)  # Coefficients at the optimal lambda

(cv_lasso)
plot(cv_lasso$glmnet.fit, xvar = "dev",label=TRUE)  # Plot the CV results
plot(cv_lasso$glmnet.fit, xvar = "lambda",label=TRUE)  # Plot the CV results

lasso.model_direct <- cv_lasso$glmnet.fit

tLL <- lasso.model_direct$nulldev - deviance(lasso.model_direct)
k <- lasso.model_direct$df
n <- lasso.model_direct$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICc # -130778780 #-114582564

BIC<-log(n)*k - tLL
BIC # -130778023

data$hr_pred_dir <- predict(cv_lasso,newx=x,s="lambda.min",gamma="gamma.min")
data$res_dir <- data$hr_pred_dir - data$hr_lead_18

hist(data$res_dir,breaks = 90)
quantile(data$res_dir,probs=c(0.01,.05,0.25,.5,.75,.95,.99))

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
  .[sample(1:nrow(data),10000),] %>%
  ggplot(aes(x=hr_lead_18,y = hr_pred_dir)) +
  theme_bw() +
  geom_point( color = "blue", alpha = .1) +
  geom_abline(slope = 1, intercept = 0, color = "red")

#####

pr <- expand.grid(
  speed = seq(4,12, length.out = 5),
  start_time_scaled = seq( 0,1,length.out = 300),
  hr_lead_18 = 0,
  time = 500
  ) %>%
  mutate( dist.90 = speed/3.6*90)

x2 <- model.matrix(form, pr)[,c(-1,-5)]

pr$pr <- predict(cv_lasso,newx=x2,s="lambda.min",gamma="gamma.min")

pr %>%
  ggplot( aes( x = start_time_scaled, y = pr, 
               color = speed, group = speed)) +
    theme_bw() +
    geom_line() +
    geom_point( data = data %>% .[sample(1:nrow(data),500),],
                mapping = aes(y = hr_lead_18),
                alpha = .1)

#####


pr <- expand.grid(
  speed = seq(4,12, length.out = 20),
  start_time_scaled = seq( 0,1,length.out = 10),
  hr_lead_18 = 0,
  time = 500
) %>%
  mutate( dist.90 = speed/3.6*90)

x2 <- model.matrix(form, pr)[,c(-1,-5)]

pr$pr <- predict(cv_lasso,newx=x2,s="lambda.min",gamma="gamma.min")

pr %>%
  ggplot( aes( x = speed, y = pr, 
               color = start_time_scaled, 
               group = start_time_scaled)) +
  theme_bw() +
  geom_line(alpha = .75)

save.image(file = here::here("inst","lasso_model2.Rdata"))

#########



knots <- unique(data$start_time_scaled)
knots <- knots[seq(40, length(knots), by=40)]


# Create a matrix of predictors and response
x <- model.matrix(hr_lead_18 ~ 
                    #splines2::mSpline(dist.90,degree=1) +
                    
                    splines2::mSpline(speed, degree = 2)
                    #+ speed
                  + hr 
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
cvrep <- 20
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

data$hr_pred_xtra_full <- predict(lasso.model_comp, newx = x)

pr <- expand.grid(
  speed = seq(2,13, length.out = 110),
  start_time_scaled = seq( 0,1,length.out = 160),
  hr_lead_18 = 0,
  hr = seq(140,220, length.out = 200)
) %>%
  mutate( dist.90 = speed/3.6*90)

x2 <-  model.matrix(hr_lead_18 ~ 
                      #splines2::mSpline(dist.90,degree=1) +
                      splines2::mSpline(speed, degree = 2)
                    #+ speed
                    + hr 
                    * splines::bs(start_time_scaled, df = dff)
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
speed_uniq <- sort(unique(data$speed))

pb <- txtProgressBar()
for (i in 1:length(speed_uniq)) {
  indexs <- which( data$speed == speed_uniq[i])
  df_act <- data[indexs,]
  pr_act <- pr %>% 
    ungroup() %>%
    filter( speed >= speed_uniq[i] - 0.05,
            speed <= speed_uniq[i] + 0.05) %>%
    select( c(start_time_scaled, pr))
  
  data_full <- fuzzyjoin::difference_left_join(
    df_act,
    pr_act,
    by= "start_time_scaled",
    max_dist = .013)
  
  data$hr_pred_xtra[indexs] <- data_full$pr
  
  setTxtProgressBar(pb, i/length(speed_uniq))
}
close(pb)

data$res <- data$hr_pred_xtra - data$hr_lead_18

hist(data$res,breaks = 90)

repmod::report(lasso.model,s=cv_lasso$lambda.min)

cor(data$hr_lead_18,data$hr_pred_xtra_full, use = "pairwise.complete.obs")
cor(data$hr_lead_18,data$hr_pred_xtra_full, use = "pairwise.complete.obs")^2


cor(data$hr_lead_18,data$hr_pred_xtra, use = "pairwise.complete.obs")
cor(data$hr_lead_18,data$hr_pred_xtra, use = "pairwise.complete.obs")^2

cor(data$hr_lead_18,data$hr_pred_dir, use = "pairwise.complete.obs")
cor(data$hr_lead_18,data$hr_pred_dir, use = "pairwise.complete.obs")^2

cor(data$hr_pred_dir,data$hr_pred_xtra, use = "pairwise.complete.obs")
cor(data$hr_pred_dir,data$hr_pred_xtra, use = "pairwise.complete.obs")^2

data %>%
  .[sample(1:nrow(data),10000),] %>%
  ggplot( aes( x = hr_pred_dir, y = hr_pred_xtra, color = 
                 hr#start_time_scaled
               )) +
  theme_bw() +
    geom_point()

data %>%
  filter( start_time_fac == favorite_session) %>%
  ggplot(aes(x=Time,y = hr_lead_18)) +
  theme_bw() +
  geom_line( color = "blue") +
  geom_line( mapping = aes(y = hr_pred_xtra_full), color = "red") +
  geom_line( mapping = aes(y = speed*10), color = "green")


