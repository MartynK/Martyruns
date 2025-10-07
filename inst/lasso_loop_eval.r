library(dplyr)

load( here::here("inst","training_history_lasso_compiled.Rdata"))

# extracting the gamma values too to see how useful the elastic net is

# Define the directory containing the files
act_dir <- here::here("inst", "training_history_lasso")

# Obtain a list of .RData files in the directory
file_list <- list.files(act_dir, pattern = "\\.Rdata$", full.names = TRUE)

# Initialize an empty vector to store gamma values
gammas <- c()

# Loop through each file in the list
for (i in 1:length(file_list)) {
  # Load the file into the R environment
  load(file_list[i])
  
  # Extract gamma.min and append to the 'gammas' vector
  gammas <- c(gammas, cv_lasso$relaxed$gamma.min)
  print(i)
}

# Now, 'gammas' will contain all the gamma.min values from each file
print(gammas)

#TODOOOO
results_df_comp_long$gammas <- NA
results_df_comp_long$gammas[5:129] <- gammas


mod <- lm(log(time_elaps) ~ 
            n 
          + n_lambda 
          + n_cross 
          + n_gamma,
        results_df_comp_long)

summary(mod)

mod %>% effects::predictorEffects() %>% plot()

library(splines)
mod2_full <- lm(error_min ~ 
             (
            ns(n,df=5) 
          + ns(n_cross,df=2) 
          + ns(n_lambda,df=2)
          + ns(n_gamma,df=2)
          )^2
          ,
          results_df_comp_long)

summary(mod2_full)

anova(mod2_full)

# hacking away unneeded interactions
mod2_red1 <- lm(error_min ~ 
                    ns(n,df=5) 
                    + ns(n_cross,df=2) 
                    #+ ns(n_gamma,df=2)
                    + ns(n_lambda,df=2)
                + ns(n, df = 5):ns(n_cross, df = 2)
                + ns(n_cross, df = 2):ns(n_lambda, df = 2)
                ,
                results_df_comp_long)

summary(mod2_red1)

anova(mod2_red1)

# hacking away nonlinear terms
mod2_red2 <- lm(lambda.min ~ 
                  ns(n,df=8) 
                + ns(n_cross,df=2) 
                + ns(n_lambda,df=1)
                + ns(n, df = 5):ns(n_cross, df = 2)
                + ns(n_cross, df = 2):ns(n_lambda, df = 1)
                ,
                results_df_comp_long)

anova(mod2_red1, mod2_red2)
anova(mod2_red2)
summary(mod2_red2)

mod2_red2 %>% effects::predictorEffects(partial.residuals = TRUE) %>% plot()

pr <- expand.grid( n = seq(10,90,length.out =  50),
                  n_cross = seq(10,33,length.out =  8),
                  n_lambda = seq(2,50,length.out =  10)
                  )
pr$pr <- predict(mod2_red2, newdata = pr)

library(ggplot2)
pr %>%
  ggplot( aes( x = n, y = pr, group = n_lambda,
               color = n_lambda)) +
    geom_line() +
    facet_wrap(facets = "n_cross")


