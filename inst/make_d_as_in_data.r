library(splines)
library(dplyr)

#######

#source(file = here::here("inst","preproc_v4.r"))
load(here::here("data","data.rdata"))

datescale <- function( datez) {
  min_d <- min(datez)
  max_d <- max(datez)
  span  <- max_d - min_d
  
  datez <- (datez - min_d) / span * 99 + 1
  return(datez)
}

load(here::here("data","pred_temps.rdata"))
preds <- preds %>%
  mutate( date =  as.Date(day_in_year, origin = paste0(years,"-01-01")),
          gluetime = ymd_h(paste(date, hour))) %>%
  left_join( y = temps_xtra %>% select(temp, datetim), by = c("gluetime" = "datetim")) %>%
  mutate( pred = ifelse(is.na(temp) == TRUE, pred, temp)) %>%
  select( gluetime, pred)

# preds$gluetime[preds$gluetime %in% temps_xtra$datetim]

d <- data %>%
  group_by( start_time_fac) %>%
  #REDUCTION!!
  mutate( chnk = ceiling(row_number()/1)) %>%
  group_by( start_time_fac, chnk) %>%
  mutate( speed = mean(speed),
          ## Windsorizing
          #speed = ifelse( speed  > 13, 13, speed),
          #speed = ifelse( speed  < 2, 2, speed),
          hr_lead_18 = mean(hr_lead_18),
          cad = mean(cad)) %>%  # STATE CHANGE
  filter(actual_run == TRUE,
         is.na(speed) == FALSE,
         ## redundant due to windsorizing
         #speed < 13,
         #speed > 2,
         start_time_fac != "1513430033",
         hr_lead_18 > 106
  ) 

d$start_time_scaled <- datescale(d$start_time) # doesnt work with mutate?

d <- d %>%
  ungroup %>%
  mutate(
    #start_time_scaled = datescale(start_time),
    year             = year(start_time_dat) %>% as.factor
    ,speed_scaled    = (speed - 2.006667) / (12.99667 - 2.006667)
    ,speed_cent = speed - mean(speed)
    ,start_time_cent = start_time_scaled - mean( start_time_scaled)
    ,lincomb = speed * 10 + start_time_scaled
  )

set.seed(123457)
grs <- unique( d$start_time_fac)
grs <- grs[ sample(1:length(grs) 
                   #,size = 80
)]

e <- d %>%
  group_by(start_time_fac, chnk)  %>%
  filter( start_time_fac %in% grs) %>%
  slice(1) %>%
  as.data.frame %>%
  group_by( start_time_fac) %>%
  mutate(time_rank = (time_num - first(time_num)) / 30,
         gluetime = round_date(start_time_dat, unit = "hours")) %>%
  left_join(y = preds, by = c("gluetime")) %>%
  rename( temp = "pred" ) %>%
  ungroup() 


mod_speedcorr <- lm(speed ~ ns(start_time_scaled,df=20), e)

#mod_speedcorr %>% effects::predictorEffects() %>% plot

e$speed_mean = predict(mod_speedcorr, newdata = e)

e <- e %>%
  # Regularizing the speeds, requires the model
  mutate(  speed_corr = speed - speed_mean )

part <- 6
len  <- floor(nrow(e)/part) 
e$part <- part
for (i in 1:part) {
  e$part[((i-1)*len+1):(i*len)] <- i
}
e$part <- as.factor(e$part)

save( e, file = here::here("data","d_as_in_data.rdata"))
