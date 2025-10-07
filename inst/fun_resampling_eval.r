library(ggplot2)
library(dplyr)
library(lubridate)

load(here::here("data","bootstrapped_hrs.rdata"))

load(here::here("data","d_as_in_data.rdata"))
sum_timediff <- as.numeric(e$start_time_dat[nrow(e)] - e$start_time_dat[1])


#plot(colnames(as.matrix(resmat)),as.matrix(resmat)[5000,],type='l')

zones <- data.frame(
  zone_no = 1:5,
  lab   = c("Z1","LT1","Z3","Z4","Z5"),
  from  = c(120 ,153  ,160 ,178 ,189),
  until = c(153 ,160  ,178 ,189 ,210),
  ratio = NA
)

resmat <- tibble(as.data.frame(resmat)) # sometimes it was converted, syntax breaks if wasnt

for (i in 1:5) {
  zones_df_act <- 
    data.frame(
      nrow = sapply( 1:ncol(resmat), function(x,lim = zones$until[i]) { 
        min(nrow(resmat), min(which(c(resmat[,x])[[1]]>lim)))
      }),
      tim  = as_datetime(as.numeric(colnames(resmat))),
      zone_lab = zones$lab[i],
      zone = zones$zone_no[i])
  
  if (i == 1) {
    zones_df <- zones_df_act
  } else {
    zones_df <- bind_rows( zones_df, zones_df_act)
  }
}

zones_df$last_zone_end <- NA
for (i in 1:nrow(zones_df)) {
  if ( zones_df$zone[i] == 1) {
    zones_df$last_zone_end[i] <- 0
  } else {
    zones_df$last_zone_end[i] <- zones_df %>% 
      filter( tim == zones_df$tim[i],
              zone == zones_df$zone[i] - 1) %>%
      .$nrow
  }
}
zones_df$rows_in_zone <- zones_df$nrow - zones_df$last_zone_end


##
zones_df <- left_join( x = zones_df, 
                        y = data.frame( 
                          tim = as_datetime(as.numeric(colnames(resmat))),
                          amount = res_amounts),
                       by = "tim") %>%
  ungroup() %>%
  group_by(tim) %>%
  mutate( hs_in_zone = amount * rows_in_zone / 3600 / nrow(resmat))

zones_df$hs_stacked <- NA

# Takes long
pb <- txtProgressBar()
for (i in 1:nrow(zones_df)) {
  zones_df$hs_stacked[i] <- zones_df %>% 
    filter( tim == zones_df$tim[i], 
            zone <= zones_df$zone[i]) %>% 
    .$hs_in_zone %>% 
    sum()
  setTxtProgressBar(pb, i/nrow(zones_df))
}
close(pb)

zones_df %>%
  ggplot( aes(x = tim, y = nrow/nrow(resmat), 
              group = zone,color = factor(zone),
              fill = zone)) +
    tidyquant::theme_tq() +
    geom_line()

###!!!
zones_df %>%
  ggplot( aes(x = tim, y = hs_in_zone,#stacked, 
              group = (desc(zone)),color = factor(zone),
              fill = factor(zone))) +
  tidyquant::theme_tq() +
  geom_area(alpha = .3) +
  scale_x_datetime(date_breaks = "2 years") +
  labs(x = "")


library(streamgraph)

zones_df %>%
  streamgraph(key = "zone", value = "hs_in_zone", date = "tim", 
              offset="zero", interpolate="step-before" ) 


