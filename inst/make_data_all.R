library(dplyr)

# loading a sample session data
load(here::here("data","d_as_in_data.rdata"))

data_all <- e %>%
  mutate(start_time_scaled = as.numeric(difftime(
    start_time_dat,
    min(start_time_dat),
    units = "days"
  )) /
    as.numeric(difftime(max(start_time_dat),
                        min(start_time_dat),
                        units = "days"
    ))) %>%
  filter(actual_run == TRUE) %>%
  select(hr, speed, Time, start_time_fac, start_time_scaled) %>%
  filter(complete.cases(.)) %>%
  filter(as.character(start_time_fac) != "1469049250")

rm(e)

favorite_session <- "1596369130"
