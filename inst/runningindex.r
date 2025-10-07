library(dplyr)
library(ggplot2)
library(splines)

run_dat <- 
  here::here("inst","extdata","run_data.xlsx") %>%
    readxl::read_xlsx() %>%
      #Date is a date...
      mutate(Date = as.Date(Date, origin = "1899-12-30"),
             hr = ifelse(as.numeric(HR) == 0, NA, as.numeric(HR)),
             date_num = as.numeric(Date)
             ) %>%
  filter(Duration > 0)

# Forward function: given hr, maxhr, dur, and dist, compute ri.
calc_ri <- function(hr, maxhr, dur, dist) {
  # Compute x element-wise, saturating at 1
  x <- pmin(1, (hr / maxhr) * 1.45 - 0.3)
  # Compute ri using the provided formula
  ri <- (213.9 / dur * dist^1.06 + 3.5) / x
  return(ri)
}

# Inverse function: given ri, maxhr, dur, and dist, recover hr.
# This inverse is defined only when the forward function did not saturate (i.e. when (hr/maxhr)*1.45 - 0.3 < 1).
calc_hr <- function(ri, maxhr, dur, dist) {
  # Compute the numerator constant from the forward function
  numerator <- 213.9 / dur * dist^1.06 + 3.5
  # Solve for x from the forward formula: x = numerator / ri
  x <- numerator / ri
  # Invert the expression for x when not saturated:
  # x = (hr/maxhr)*1.45 - 0.3  -->  hr = maxhr * ((x + 0.3) / 1.45)
  hr <- maxhr * ((x + 0.3) / 1.45)
  
  # If x is >= 1, the original forward function was saturated,
  # so the inverse is not uniquely defined. Return NA in those cases.
  # hr[x >= 1] <- NA
  return(hr)
}

# Example usage:
# Create a sample data frame
dat <- expand.grid(
  maxhr = 194,
  hr = seq(150, 175),
  dur = 60,
  dist = 7.5
)

# Compute ri using the forward function
dat <- dat %>% 
  mutate(
    ri = calc_ri(hr, maxhr, dur, dist)
  )

# Example usage:
# Create a sample data frame
dat <- expand.grid(
  maxhr = 194,
  ri = seq(36,36,1),
  speed_delta = seq(-0.3,0.3,0.1),
  dur = seq(12,80,length.out=20)
) %>% 
  mutate(
    speed = 0 - 0.9*log(dur) + 10.78 + speed_delta,
    dist = speed * dur / 60,
    hr = calc_hr(ri, maxhr, dur, dist)
  )

#plot
dat %>% 
  ggplot(.,aes(x = dur, y = speed,
               color = hr, group = paste0(ri,speed_delta))) +
  theme_minimal() +
  geom_point() +
  geom_line() +
  labs(x = "Time", y = "Speed")


# Do the inverse, eg. what speed for a constant 150, 1660 etc. hr

calc_dist <- function(hr, maxhr, dur, ri) {
  # Compute x element-wise, saturating at 1
  x <- pmin(1, (hr / maxhr) * 1.45 - 0.3)
  
  # Calculate the numerator for the dist equation:
  # We need (ri * x - 3.5) to be positive.
  numerator <- ri * x - 3.5
  
  # Warn if any values lead to a negative or zero numerator and return NA for them
  if(any(numerator <= 0)) {
    warning("For some values, ri*x <= 3.5. dist will be set to NA for these cases.")
  }
  
  # Compute dist using the inverse relation:
  dist <- ifelse(numerator > 0,
                 ((dur / 213.9) * numerator)^(1/1.06),
                 NA)
  return(dist)
}

dat_x <- data.frame(hr = seq(60,194)) %>% 
  mutate(
    hr_perc = hr/194,
    hr_res  = (hr-60)/(194-60),
    x = pmin(1,(hr/194)*1.45-0.3))

dat_x %>%
  ggplot(aes(x = hr, y = x)) +
  theme_minimal() +
  geom_line() +
  geom_hline(yintercept = 1, color = "salmon4") +
  scale_x_continuous(breaks = seq(60,194,by=10)) +
  geom_point()

dat_y <- expand.grid(
  dur = seq(10,300,by=10),
  speed = seq(5,9,by=1)
) %>% 
  mutate(
    dist = speed * dur / 60,
    ri_y = (213.9 / dur) * dist^1.06 + 3.5,
    hr   = calc_hr(40, 194, dur, dist)
  ) %>%
  filter(dist <= 25)

dat_y %>%
  ggplot(aes(x = dur, y = ri_y, 
             color = hr,
             group = speed)) +
  theme_minimal() +
  geom_point() +
  geom_line()

dat_y %>%
  ggplot(aes(x = dist, y = ri_y, 
             color = hr,
             group = speed)) +
  theme_minimal() +
  geom_point() +
  geom_line()

dat2 <- expand.grid(
  maxhr = 194,
  hr = c(150,160,170,180),
  dur = c(seq(6,180,by=6),36.4,75.2,170),
  ri = seq(36,39,by=1)
  ) %>% 
  mutate(
    dist = calc_dist(hr, maxhr, dur, ri),
    speed = dist / dur * 60
  ) %>%
  filter(
    !(hr >= 160 & dur > 160),
    !(hr >= 165 & dur > 90),
    !(hr >= 175 & dur > 30),
    !(hr >= 185 & dur > 10)
  )


#plot
dat2 %>% 
  ggplot(.,aes(x = dur, #same with speed
               y = speed, color = hr, group = paste0(ri,hr))) +
  theme_minimal() +
  geom_point() +
  geom_point(data = run_dat,color="red",mapping=aes(x=Duration,y=Speed,color=HR,group="")) +
  geom_line() +
  scale_x_sqrt(breaks = c(0,6,12,20,40,60,90,120,180)) +
  labs(x = "Time", y = "speed") +
  geom_hline(yintercept=7.1,color="salmon4") +
  facet_wrap(~ri)


dat2 %>% 
  ggplot(.,aes(x = dur, #same with speed
               y = speed, color = hr, group = paste0(ri,hr))) +
  theme_minimal() +
  geom_point(alpha = .3) +
  geom_point(data = run_dat, shape = 23,
             mapping=aes(x=Duration,y=Speed,fill=hr,group="")) +
  geom_line() +
  scale_x_sqrt(breaks = c(0,6,12,20,40,60,90,120,180)) +
  labs(x = "Time", y = "speed") +
  geom_hline(yintercept=7.1,color="salmon4") +
  geom_hline(yintercept=8.4,color="salmon4",linetype="dashed") +
  facet_wrap(~ri)

dat2 %>% 
  filter( hr %in% c(150,160,170),
           ri == 37) %>%
  ggplot(.,aes(x = dur, #same with speed
               y = speed, color = hr, group = paste0(ri,hr))) +
  theme_minimal() +
  geom_point(data = run_dat, shape = 23, size = .5,
             mapping=aes(x=Duration,y=Speed,fill=hr,group="")) +
  geom_line() +
  scale_x_sqrt(breaks = c(0,6,12,20,30,45,60,90,120,180)) +
  scale_y_continuous(limits=c(5,10)) +
  labs(x = "Time", y = "speed") +
  geom_hline(yintercept=7.1,color="salmon4")


run_dat_clean <- run_dat %>% 
  filter(Duration >= 6,
         RI > 20)
mod <- lm(RI ~ ns(date_num,df=4),
          weights = sqrt(Duration),
          run_dat_clean)
mod %>% effects::predictorEffects(partial.residuals=TRUE) %>% plot()

library(mgcv)
mod_gam <- gam(RI ~ s(date_num,bs="cr"),
               weights = sqrt(Duration),
               data = run_dat_clean)

summary(mod_gam)
plot(mod_gam)


