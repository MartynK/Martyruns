library(ggplot2)
library(splines2)
library(dplyr)
#library(mgcv)

load( file = here::here( "data", 
                         "d_as_in_data.Rdata"))

load( file = here::here( "inst", 
                         "training history_compiled",
                         "al_guessed_curves.Rdata"))


for (file in 
     list.files(here::here("R"), pattern = "\\.[rR]$", full.names = TRUE)) {
  source(file)
}


# Define the tricube weighting function
tricube_mod <- function(x) {
  weight <- (1 - abs(x)^3)^3
  
  if (weight > 0.001) {
    return(weight)
  } else if ( weight < -3) {
    return( 10^weight)
  } else {
    return(0.001)
  }
}
Tricube_mod <- Vectorize(tricube_mod)


objective_spl <- function(x,data,weights) {

  devs <- (data$hr_eq - custom_curve(x_in = data$speed,
                                     custom_coefs = x))^2 * weights
  return( sum( devs))
}

change_date_scale <- function( input, direction = "to_date", e. = e) {
  d <-  as.numeric(as.character(summary(e.$start_time_dat)[c(1,6)]))
  d <-  d[1] + (d[2] - d[1]) * input
  return(d)
  
}

all_guessed_curves <- all_guessed_curves %>% 
  mutate( start_time_dat = lubridate::as_datetime(
                      change_date_scale(start_time_scaled)))

sess_s <- unique(
  all_guessed_curves$start_time_scaled)

SESS_PER_POINT <- 6
SCALE_POINTS   <- 12

point_scales <- data.frame(
  midpoint = c(),
  scale = c()
)
for (i in 1:floor(length(sess_s)/SESS_PER_POINT)) {
  
  if (i == 1) {
    from = 0
  } else {
    from <- sess_s[SESS_PER_POINT * (i-1)]
  }
  to   <- sess_s[min( length(sess_s), 
                      SESS_PER_POINT * i)]
  
  midpt =  (to + from) / 2
  
  from_scale_ind <- last(which(sess_s < midpt)) - ceiling( SCALE_POINTS / 2)
  
  if (from_scale_ind < 1) {
    from_scale <- midpt
  } else {
    from_scale <- midpt - sess_s[from_scale_ind]
  }
  
  to_scale_ind <- first(which(sess_s > midpt)) + ceiling( SCALE_POINTS / 2)
  
  
  if (to_scale_ind > length(sess_s)) {
    to_scale <- 1 - midpt
  } else {
    to_scale <- sess_s[to_scale_ind] - midpt
  }
  
  scale <- max( from_scale, to_scale)
  
  
  
  point_scales <- bind_rows( point_scales,
                             data.frame(
                               midpt = midpt,
                               scale = scale
                             ))
}

plot(point_scales$midpt,point_scales$scale)
point_scales$id <- 1:nrow(point_scales)

ITER_CRITICAL <- 300
lower_bounds <- c(100,0,0)
upper_bounds <- c(100,5,5)



pr <- expand.grid( speed = seq(5,12, length.out = 8),
                   id = point_scales$id,
                   dp = 0) 

pr <- left_join( pr, point_scales, by = "id")


pb <- txtProgressBar()
for (i in 1:nrow(pr)) {
  pr$dp[i] <- all_guessed_curves %>%
    filter( speed > pr$speed[i] - 0.1,
            speed < pr$speed[i] + 0.1,
            start_time_scaled > pr$midpt[i] - pr$scale[i],
            start_time_scaled < pr$midpt[i] + pr$scale[i]
    ) %>% nrow()
  setTxtProgressBar(pb, i/nrow(pr))
}
close(pb)


pr <- pr%>%
  mutate(start_time_dat = lubridate::as_datetime(
    change_date_scale(midpt)))
pr$pr <- NA
distinct_timez <- pr %>% 
  group_by( midpt) %>% 
  slice(1)


pb <- txtProgressBar()
for (i in 1:nrow(distinct_timez)) {
  
  weights <- Tricube_mod( (distinct_timez$midpt[i] - 
                            all_guessed_curves$start_time_scaled) /
                            (distinct_timez$scale[i]))

  if (  T==T){#sd(weights) > 0.01) {

    op <- nloptr::nloptr(
      x0 = c(100,0.5,1),            # Initial guess for 'c'
      eval_f = objective_spl,         # Objective function to minimize
      data = all_guessed_curves,
      weights = weights,
      lb = lower_bounds,          # Lower bounds for the parameters
      ub = upper_bounds,          # Upper bounds for the parameters
      opts = list(
        algorithm = "NLOPT_LN_NELDERMEAD",  # Optimization algorithm (e.g., COBYLA)
        maxeval = ITER_CRITICAL,             # Maximum number of function evaluations
        ftol_rel = 1e-6,            # Relative function tolerance for convergence
        print_level = 0
        #local_opts = list(algorithm = "NLOPT_LN_BOBYQA")
    ))
    
    guesses <- 
      custom_curve(x_in = pr$speed[pr$midpt == distinct_timez$midpt[i]],
                   custom_coefs = op$solution)
    
    pr$pr[pr$midpt == 
            distinct_timez$midpt[i]] <- guesses
  }
  
  setTxtProgressBar(pb, i/nrow(distinct_timez))
}
close(pb)

pr <- pr %>% filter( is.na(pr) == FALSE)


pr %>%
  ggplot(aes( x = midpt, y = pr, 
              color = speed, group = speed)) +
    theme_bw() +
    geom_point(size = 2) +
    geom_line(size = 1) +
    geom_point( data = all_guessed_curves %>%
                  mutate(speed = round(speed,digits = 1)) %>%
                  filter(speed %in% c(6,8,9,10)),
                mapping = aes( y = hr_eq, #shape = as.integer(speed)
                               ),
                size = .5) +
  geom_smooth(  data = all_guessed_curves %>%
                  mutate(speed = round(speed,digits = 1)) %>%
                  filter(speed %in% c(6,8,9,10)),
                mapping = aes( y = hr_eq, #shape = as.integer(speed)
                ), linetype = "dashed", se = FALSE,
                method = "loess", span = 0.1) +
  scale_y_continuous(limits = c(0,250))

pr %>%
  ggplot(aes( x = midpt, y = pr, 
              color = speed, group = speed)) +
  theme_bw() +
  geom_line(size = 2) +
  geom_point( data = all_guessed_curves %>%
                mutate(speed = round(speed,digits = 1)) %>%
                filter(speed %in% c(6,8,9,10)),
              mapping = aes( y = hr_eq, #shape = as.integer(speed)
              ),
              size = .5) +
  scale_y_continuous(limits = c(0,350))


pr %>%
  filter(midpt >= 0.95,
         dp > 0) %>%
  ggplot(aes( x = speed, y = pr, 
              color = start_time_dat, 
              group = midpt)) +
  theme_bw() +
  geom_line(size = 1.5) +
  geom_line( data = all_guessed_curves %>%
                filter(start_time_scaled >= 0.95),
              mapping = aes( y = hr_eq,
                             group = start_time_scaled),
              size = .175,
              alpha = 1.0,
              linetype = "dashed"
             ) +
  scale_y_continuous(limits = c(0,350))



#mod %>% effects::predictorEffects() %>% plot()


