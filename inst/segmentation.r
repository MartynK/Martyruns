library(ggplot2)
library(dplyr)
library(lubridate)

# for (file in 
#      list.files(here::here("R"), pattern = "\\.[rR]$", full.names = TRUE)) {
#   source(file)
# }

source(here::here("inst","make_data_all.R"))

data_all$date <- data_all$start_time_fac %>% 
  as.character() %>% 
  as.numeric() %>% 
  as.POSIXct(., origin = "1970-01-01", tz = "UTC")

# Extract year as a numeric value
data_all$year <- year(data_all$date)

# Extract ISO week of the year (numeric)
data_all$week <- isoweek(data_all$date)


min_date <- ymd(paste0("2024-12-01"))         # January 1st of the specified year

# Define minimum and maximum dates using lubridate
wks <- difftime(today(), min_date, units = "weeks") %>% 
  as.numeric() %>% 
  ceiling()
  
max_date <- today() #min_date + weeks(wks)            # min_date plus wk_wdth weeks

grps <- 
  data_all %>% 
    filter(date >= min_date, date < max_date) %>%
    group_by(year, week,speed) %>%
    summarise(n = n())

mx_width  <- grps$n %>% max()

date_first <-
  data_all %>% 
    filter(date >= min_date) %>% 
    arrange(date) %>% 
    .[1,] %>% 
    .$date %>%
    floor_date(.,unit = "week")

date_last <-
  today() #date_first + weeks(wks)

pb <- txtProgressBar(min = 1, max = wks, style = 3)
for (i in 1:wks) {
  data_chnk <- 
    data_all %>% 
    filter(date >= date_first + weeks(i - 1),
           date < date_first + weeks(i))
  
  if(nrow(data_chnk) == 0) {
    next
  }
  
  # get minmax speeds to make embedded loop more efficient
  xtrm_speeds <- data_chnk$speed %>% range()
  
  for (j in seq(xtrm_speeds[1],
                xtrm_speeds[2],
                by = .1)) {
    act_data <- 
      data_chnk %>% 
      filter(speed == j) 
    
    act_out<- tibble(
      speed = j,
      wk = i - 1,
      hr = act_data %>%
        .$hr %>%
        { replace(., is.na(.), mean(., na.rm = TRUE)) } %>%
        sort(),
      act_x = 
        seq(-nrow(act_data)/mx_width/2 + (i - 1),
            nrow(act_data)/mx_width/2 + (i - 1), 
            length.out = nrow(act_data))
    )
    
    if( exists("data_trf")) {
      data_trf <-  bind_rows(data_trf, act_out)
    } else {
      data_trf <-  act_out
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)

data_trf <- data_trf %>% 
  # transform 'wk_hmr' to human readable
  mutate(wk_hmr = min_date + weeks(wk))

data_trf %>%
  ggplot(aes(x = act_x, y = speed, 
             group = paste0(speed,wk), color = hr)) +
  #geom_line() +
  geom_point(alpha=.1) +
  theme_minimal() +
  geom_hline(yintercept = 6, color = "salmon4") +
  geom_hline(yintercept = 7.1, color = "salmon4",linetype="dashed") +
  scale_colour_gradient2(low = "blue", mid = "green",  high = "red", 
                         limits = c(120,190),
                         midpoint = 157) +
  scale_y_continuous( breaks = c(5,6,7,8,9,12)) +
  scale_x_continuous( breaks = seq(0,wks, length.out = 8),
                      labels = seq(min_date,max_date,length.out = 8)
                      ) +
  labs(x = "") +
  theme(legend.position = "bottom") 

