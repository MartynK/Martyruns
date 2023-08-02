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


SCALE <- 0.002
ITER_CRITICAL <- 300
lower_bounds <- c(100,0,0)
upper_bounds <- c(100,5,5)

plot( seq(0,-1,length.out = 200),
      Tricube_mod(seq(0,-1,length.out = 200)/(SCALE*1)),
      type = "l",
      xlim = c(-.1,0))

pr <- expand.grid( speed = c(6,8,9,10),
                   start_time_scaled = seq(0,1,length.out = 10),
                                             #ceiling(1/(SCALE*(1/2)))
                   dp = 0) 

pr2 <- expand.grid( speed = seq(6,12, length.out = 20),
             start_time_scaled = seq(0.90,1,length.out = 10 ),
             dp = 0)

pr <- bind_rows(pr,
                pr2)

pb <- txtProgressBar()
for (i in 1:nrow(pr)) {
  pr$dp[i] <- all_guessed_curves %>%
    filter( speed > pr$speed[i] - 0.1,
            speed < pr$speed[i] + 0.1,
            start_time_scaled > pr$start_time_scaled[i] - 0.025,
            start_time_scaled < pr$start_time_scaled[i] + 0.025
    ) %>% nrow()
  setTxtProgressBar(pb, i/nrow(pr))
}
close(pb)


pr <- pr%>%
  mutate(start_time_dat = lubridate::as_datetime(
    change_date_scale(start_time_scaled)))
pr$pr <- NA
distinct_timez <- unique(pr$start_time_scaled)


pb <- txtProgressBar()
for (i in 1:length(distinct_timez)) {
  
  weights <- Tricube_mod( (distinct_timez[i] - 
                            all_guessed_curves$start_time_scaled) /
                            (SCALE*1))

  if (  sd(weights) > 0.01) {

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
      custom_curve(x_in = pr$speed[pr$start_time_scaled == 
                                                     distinct_timez[i]],
                   custom_coefs = op$solution)
    
    pr$pr[pr$start_time_scaled == 
            distinct_timez[i]] <- guesses
  }
  
  setTxtProgressBar(pb, i/length(distinct_timez))
}
close(pb)

pr <- pr %>% filter( is.na(pr) == FALSE)


surf <- expand.grid(p2 = seq(0,1.25,length.out = 20),
                    p3 = seq(0,2,length.out = 8),
                    dev = NA
                    )

pb <- txtProgressBar()
for (i in 1:nrow(surf)) {
  surf$dev[i] <- objective_spl(c(100,surf$p2[i],
                                 surf$p3[i]), 
                               data = all_guessed_curves,
                               weights = weights)
  setTxtProgressBar(pb, i/nrow(surf))
}
close(pb)

surf %>%
  ggplot(aes(x = p2, y = log(dev), 
             group = p3, color = p3)) +
  theme_bw() +
  geom_line()


pr %>%
  ggplot(aes( x = start_time_scaled, y = pr, 
              color = speed, group = speed)) +
    theme_bw() +
    geom_line(size = 2) +
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
  ggplot(aes( x = start_time_scaled, y = pr, 
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
  filter(start_time_scaled >= 0.9,
         dp > 0) %>%
  ggplot(aes( x = speed, y = pr, 
              color = start_time_dat, 
              group = start_time_scaled)) +
  theme_bw() +
  geom_line(size = 1.5) +
  geom_line( data = all_guessed_curves %>%
                filter(start_time_scaled >= 0.95),
              mapping = aes( y = hr_eq),
              size = .175,
              alpha = 1.0,
             linetype = "dashed") +
  scale_y_continuous(limits = c(0,350))



#mod %>% effects::predictorEffects() %>% plot()

sess_s <- unique(
  all_guessed_curves$start_time_scaled)

SESS_PER_POINT <- 6
SCALE_POINTS   <- 18

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
