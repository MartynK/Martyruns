library(dplyr)
library(lubridate)
library(readr)

preproc_file <- function(fname) {
  require(lubridate)  
  
  suppressMessages(
    {
      data_pr <- read_csv(fname, skip = 2, col_types = cols(), progress = FALSE)
      
      session_time <- read_csv(fname, 
                               skip = 0, 
                               n_max = 1, 
                               col_types = cols(), progress = FALSE)[1,3:4] 
    }
  )

  session_time <-  dmy(session_time[[1]]) %>% 
                         as_datetime() + session_time[[2]]
  
  data_pr <- data_pr %>%
    select( -`Sample rate`,
            -`Power (W)`,
            -`Temperatures (C)`,
            #-`X12`,
            -`Pace (min/km)`) %>%
    rename( "hr" = `HR (bpm)`,
            "speed" = `Speed (km/h)`,
            "stride" = `Stride length (m)`,
            "dist" = `Distances (m)`,
            "cad" = `Cadence`,
            "alt" = `Altitude (m)`) %>%
    mutate( time_num = Time %>% as.numeric(),
            stride = speed / cad,
            hr_lead_18 = lead(hr,18),
            start_time = session_time)
  
  
  lag_cor <- function(x, variab = "hr", variab_a = "speed", dir = lead) {
    data_pr %>%
      select( variab_a, variab) %>%
      mutate( y = dir(data_pr %>% select(all_of(variab)) , x)) %>%
      cor(., use = "complete.obs") %>%
      .[1,3]
  }
  
  #sapply( 0:30, "lag_cor") %>% plot(.,type = 'l')
  #sapply( 0:30, FUN = function(x) {lag_cor(x,"cad","hr.lead.17",lag)}) %>% plot(.,type = 'l') 
  # #cadence: lag of 7 compared to HR.lead, which is lagged 17, so.... 10
  
  
  data_pr$dist.90 <- sapply( 1:nrow(data_pr), function(x){
    if (x <= 90) {
      return(0)
    } else {
      return(data_pr$dist[x] - data_pr$dist[x-90])
    }
  })
  data_pr$speed.90 <- data_pr$dist.90 / 90 * 3.6
  
  #hist(data_pr$speed.90,breaks =300)
  
  data_pr$actual_run <- TRUE
  data_pr$actual_run[1:90] <- FALSE
  #data_pr$actual_run[dat_pr$speed.90<3] <- FALSE
  
  data_pr$actual_run <- sapply(1:nrow(data_pr), function(x){
    if ( data_pr$actual_run[x] == FALSE) {
      return(FALSE)
    } else {
      if ( min(data_pr$speed.90[x:min((x+90),nrow(data_pr))],na.rm=TRUE) < 3.5) {
        return( FALSE)
      } else {
        return(TRUE)
      }
    }
  })
  
  # par(mfcol=c(3,1))
  # plot(data_pr$speed)
  # plot(data_pr$speed.90)
  # plot(data_pr$actual_run, type = 'l')
  # par(mfcol=c(1,1))
  
  return(data_pr)
}

filez <- here::here("inst","extdata","csv_data") %>% list.files()

pb <- txtProgressBar(min = 0, max = length(filez), style = 3)
for (i in 1:length(filez)) {
  if (i == 1){
    data <- paste0(here::here("inst","extdata","csv_data"),
                   "/",
                   filez[i]) %>% 
              preproc_file() 
  } else {
    data <- paste0(here::here("inst","extdata","csv_data"),
                   "/",
                   filez[i]) %>% 
      preproc_file() %>%
          rbind(data, .)
  }
  setTxtProgressBar(pb, i)
}
close(pb)

#summary(data$cad[data$actual_run==T])

data <- data %>%
           mutate( start_time_dat = start_time,
                   start_time = start_time %>% as.numeric(),
                   start_time_fac = start_time %>% as.factor(),
                   cad = ifelse( is.na(cad) == TRUE, 70, cad))

# # Dg. plot
# data %>%
#   ggplot( aes(x = time_num, y = speed, group = start_time_fac)) +
#   geom_line() +
#   facet_wrap(facets="start_time_fac") +
#   geom_line(aes(y=actual_run %>% as.numeric()),color='red',size=2) +
#   scale_y_continuous(limits=c(.1,NA))


datescale <- function( datez) {
  min_d <- min(datez)
  max_d <- max(datez)
  span  <- max_d - min_d
  
  datez <- (datez - min_d) / span * 100
  return(datez)
}

## 18, 17 thet two best options
#out <- c()
#for (i in 1:50) {

CONCAT <- 18

data_act <- data %>%
  mutate(start_time_scaled = datescale(start_time)) %>%
  group_by(start_time) %>%
  mutate( chunkno = min_rank(time_num),
          yr = year(start_time_dat) %>% factor(),
          concatno = floor( chunkno / CONCAT ) %>% as.factor) %>%
  filter( actual_run == TRUE) %>%
  group_by(start_time, concatno) %>%
          mutate( hr_con = mean(hr, na.rm = TRUE),
                  hr_lead_18_con = mean(hr_lead_18, na.rm = TRUE),
                  speed_con = mean(speed, na.rm = TRUE),
                  dist_con = mean(dist, na.rm = TRUE),
                  time_num_con = mean(time_num, na.rm = TRUE),
                  cad_con = mean(cad, na.rm = TRUE)
                  ) %>%
  filter(row_number() == 1)

cor_lag <- ccf(data_act$hr_con,data_act$speed_con,na.action=na.pass)

max_cor <- cor_lag$lag[which(cor_lag$acf == max(cor_lag$acf))]

data_act <- data_act %>%
              ungroup( concatno) %>%
              mutate( hr_con_shift = lead( hr_con, n = max_cor))

# #hist(data_act$speed_con,breaks=100)
# #quantile(data_act$speed_con,probs=c(.1,.995),na.rm=TRUE)

data_act <- data_act %>%
  mutate( speed_con_corr = ifelse( speed_con>12,12,speed_con),
          actual_run = ifelse( speed_con_corr>=4.5, actual_run, FALSE),
          time_num_con_r = round(time_num_con )) %>%
  filter(actual_run == TRUE)

# ccf(data_act$hr_con_shift,data_act$speed_con,na.action=na.pass)

#out <- c(out, cor(data_act$hr_con_shift,data_act$speed_con,use="complete.obs"))

#}

## shift of 18 sec. was indicated!!!
save(data, data_act, file=here::here("data","data.rdata"))
beepr::beep()
