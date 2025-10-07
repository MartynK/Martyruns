library(dplyr)
library(ggplot2)
library(splines)


load( here::here( "data", "d_as_in_data.rdata"))

set.seed(123)

big_d <- e
  # e %>%
  # filter(
  #   #speed > 7
  #   start_time_fac  %in% sample(unique(e$start_time_fac),160)#== "1688559552"
  # ) 

for (i in 1:length(unique(big_d$start_time_fac))) {
  act_start_time_fac <- unique(big_d$start_time_fac)[i]
  
  small_d <- 
    e %>%
    filter(
      #speed > 7
      start_time_fac == act_start_time_fac
    ) %>%
    mutate(
      big_gap_ahead = lead(Time,1) - Time 
    )
  small_d$big_gap_ahead[nrow(small_d)] <- 1
  
  
  curr_bin_start <- 750#small_d$dist[1] + 250
  curr_bin_no    <- 1
  bins           <- rep(NA, nrow(small_d))
  
  for (j in 1:nrow(small_d)) {
    
    change_bins <- FALSE
    
    if ( small_d$big_gap_ahead[j] > 45) {
      # big gap ahead
      bins[j]        <- curr_bin_no
      change_bins    <- TRUE
      
    } else if ( small_d$dist[j] > curr_bin_start + 500 ) {
      # ceiling reached
      change_bins    <- TRUE
    } else if ( j== nrow(small_d)) {
      # has to check if last chunk long enough
      change_bins    <- TRUE
    }
    
    if ( change_bins == TRUE) {
      # bins should be incremented
      
      # checking if no.obs is good (bit redundant)
      if (length(bins[bins==curr_bin_no])<120 
          |small_d$dist[j] < curr_bin_start + 450
      ) {
        bins[bins==curr_bin_no] <- NA
      }
      
      curr_bin_no    <- curr_bin_no +1
      curr_bin_start <- small_d$dist[min(j+1,nrow(small_d))]
      
    } else {
      bins[j] <- curr_bin_no
    }
  }
  
  small_d$bins <- bins  
  
  small_d <- small_d %>%
    filter( is.na(bins) == FALSE) %>%
    group_by( bins) %>%
    mutate(hr_ave = mean(hr, na.rm = TRUE),
           speed_ave = mean(speed, na.rm = TRUE)
    ) %>%
    slice(1)
  
  if ( exists("d_all")) {
    d_all <- bind_rows( d_all, small_d)
  } else {
    d_all <- small_d
  }
  print(i)
}

# d_all <- d_all %>%
#   filter(speed_ave < 12.5) # 'obvious'? outliers (2017 confirmed)

d_all %>%
  ggplot(aes(y = hr_ave,
             x = speed_ave,
             color = bins)) +
  theme_bw() +
  geom_point() +
  geom_smooth(
    method = 'lm'
    ) +
  geom_smooth(col="green")


mod <- lm( hr_ave ~ ns(speed_ave,df=1), d_all)

cd <- cooks.distance(mod) %>% sort(decreasing = TRUE)
hist(cd,breaks = 40)
head(cd, n = 10)
potential_outliers <-  head(cd, n = 10) %>% names %>% as.numeric %>% d_all[.,]

act <- 4
e %>%
  filter(start_time_fac == 
           potential_outliers$start_time_fac[act]) %>%
  ggplot(aes(x=Time,y=hr)) +
    theme_bw() +
    geom_point(col="red") +
    geom_point(mapping = aes( y = speed*10), col = "blue") +
    geom_point( data = d_all %>%
                  filter(start_time_fac == 
                           potential_outliers$start_time_fac[act]),
                mapping = aes( y = speed_ave*10),
                col = "cyan") +
    geom_point( data = d_all %>%
                  filter(start_time_fac == 
                           potential_outliers$start_time_fac[act]),
                mapping = aes( y = hr_ave),
                col = "salmon4")


mod2 <- lm( hr_ave ~ 
             ns(speed_ave,df=2)
            + ns( start_time_scaled,df=15)
           ,d_all)
summary(mod2)
plot(effects::predictorEffects(mod2))


# Dropped approaches

# Initialize variables
window_size <- 30  # Size of the sliding window
threshold   <- 3.5   # Standard deviation threshold for low variability

# Sliding window function to calculate standard deviation
sliding_window_sd <- function(vec, window_size) {
  sapply(seq_along(vec), function(i) {
    start = max(1, i - window_size + 1)
    end = i
    sd(vec[start:end])
  })
}

# Calculate standard deviation for each window
sds <- sliding_window_sd(small_d$hr, window_size)

hist(sds,breaks=40)
quantile(sds,na.rm=TRUE, probs = c(.5,.75,.8,.9,.95,.99))
# for hr ~5 is 1:20 ~ 1 over 10 mins
# for speed ~ 1.75 is similar


# Identify low variability segments
small_d <- small_d %>%
  mutate(low_var_segment = ifelse(sds <= threshold, 1, 0))


small_d %>%
  ggplot( aes( x = Time, y = hr, color = low_var_segment)) +
    theme_bw() +
    geom_point() +
    geom_point( mapping = aes(y = speed))
