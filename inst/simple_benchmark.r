library(dplyr)

load( here::here("data","d_as_in_data.rdata"))

f <- e %>%
  filter(actual_run == TRUE)

# lag = 0 best
lags <- ccf(f$dist.90,f$hr,na.action=na.pass)

# Massive autocorrelation did it to prove a point
mod <- lm( hr ~ splines::ns(speed,df=2) *
             splines::ns(start_time_scaled,df=50), f)

su <- summary(mod)

su

su$r.squared

# R = 0.7576
sqrt(su$r.squared)

f <- f %>%
  filter( is.na(speed) == FALSE,
          is.na(hr) == FALSE)
unique_session <- unique(f$start_time_fac)
out <- cbind( f[0,], data.frame(pr = c()))
pb <- txtProgressBar()
for (i in 1:length(unique_session)) {
   dat_chnk <- f %>% filter( start_time_fac == unique_session[i])
   mod_act <- lm( hr ~ splines::ns(speed,df = 2), 
                  dat_chnk)
   
   dat_chnk$pr <- predict(mod_act)
   
   suppressMessages({
   out <- bind_rows( out, dat_chnk)
   })
   
   setTxtProgressBar(pb, i/length(unique_session))
}
close(pb)

cor( out$hr, out$pr)
cor( out$hr, out$pr)^2


