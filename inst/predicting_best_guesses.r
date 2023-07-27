
for (file in 
     list.files(here::here("R"), pattern = "\\.[rR]$", full.names = TRUE)) {
  source(file)
}


source(here::here("inst","make_data_all.R"))
load(here::here("inst","training history_compiled","results.Rdata"))


GGally::ggpairs(best_guesses[,c(1:6,8:10,12)]) +
  theme_bw()

for (i in 1:length(results2)) {
  
  if (i == 1) {
    resmat <- c(results2[[i]][2]$optimization$solution,
                results2[[i]][2]$optimization$objective,
                as.numeric(as.character(results2[[i]][1]$start_time_fac)))
    
  }  else {
    resmat <- rbind(resmat,
                    c(results2[[i]][2]$optimization$solution,
                      results2[[i]][2]$optimization$objective,
                      as.numeric(as.character(results2[[i]][1]$start_time_fac))))
  }
}
resmat <- as.data.frame(resmat)

pb <- txtProgressBar()
for (i in 1:length(results2)) {
  data_act <- data_all %>% 
    filter(start_time_fac == results2[[i]][1]$start_time_fac) %>%
    mutate(Time = as.numeric(Time))
  
  restable <- return_the_predictions(
    data_all = data_act,
    x = as.numeric(as.character(resmat[i,1:(ncol(resmat)-1)]))
  )
  
  data_act <- left_join(data_act, 
                        restable %>% 
                          dplyr::select(c(Time, hr_pred)),
                        by="Time")
  
  if (i == 1) {
    data_out <- data_act
  } else {
    data_out <- bind_rows( data_out, data_act)
  }
  setTxtProgressBar(pb, i/nrow(resmat))
}
close(pb)

cor(data_out$hr,data_out$hr_pred, use = "pairwise.complete.obs")
cor(data_out$hr,data_out$hr_pred, use = "pairwise.complete.obs")^2
downsamp <- sample(1:nrow(data_out),size = 200000)

data_out[downsamp,] %>%
  ggplot( aes(x=hr,y=hr_pred
              #,group=start_time_fac
              )) +
    theme_bw() +
    geom_point(alpha=0.1,size=.5) +
    geom_smooth(color="green") +
    geom_abline(slope=1,intercept=0,color="red")

# data_out %>%
#   ggplot(aes(x=Time,y=hr)) +
#     theme_bw() +
#     geom_point() +
#     geom_point(mapping=aes(y=hr_pred),color = "red") +
#     facet_wrap(facets="start_time_scaled",scales="free_x")