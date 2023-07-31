library(dplyr)
library(splines2)
library(ggplot2)
library(manipulate)

# https://jds-online.org/journal/JDS/article/1243/file/pdf

for (file in 
     list.files(here::here("R"), pattern = "\\.[rR]$", full.names = TRUE)) {
  source(file)
}

custom_curve <- function( xmin = 0, 
                          xmax = 1,
                          x_in = seq(0,1, length.out = 10),
                          df = 3,
                          custom_coefs = c(100, 0.5, 1.3,0.7)) {
  
  dat_simp <- data.frame(
    x = seq(0,1,length.out=10))
  dat_simp$y <- (dat_simp$x * 0.1)^2
  

  mod <- lm( y ~ splines2::iSpline(x, df = 2, degree = df, intercept = FALSE)
             , data = dat_simp)
  
  mod$coefficients <- custom_coefs
  
  return(predict(mod, newdata = data.frame( x = x_in)))
  
}

example_curve <- data.frame( y =
                               custom_curve(x_in = seq(0,1, length.out = 100),
                                            df = 2,
                                            custom_coefs = c(141, 80, 111.9)))

example_curve$x <- seq(4,12,length.out = nrow(example_curve))


manipulate({
  
  example_curve <- data.frame(
    y = custom_curve(
      x_in = seq(0, 1, length.out = 100),
      df = 2,
      custom_coefs = c(coef1, coef2, coef3)
    )
  )
  
  example_curve$x <- seq(0, 12, length.out = nrow(example_curve))
  
  plot(example_curve$x, example_curve$y)
}, 
coef1 = slider(0, 300, initial = 60), 
coef2 = slider(0, 200, initial = 160), 
coef3 = slider(0, 200, initial = 00))

# Call the manipulate function
manipulate({
  example_curve <- data.frame(
    y = custom_curve(
      x_in = seq(0, 1, length.out = 100),
      df = 2,
      custom_coefs = c(coef1, coef2, coef3)
    )
  )
  
  example_curve$x <- seq(0, 12, length.out = nrow(example_curve))
  
  # Plot the curve
  example_curve %>%
    ggplot(aes(x =x, y = y)) +
    theme_bw() +
    geom_line() +
    scale_y_continuous(breaks = c(100,120,140,160,180,200,220))
}, 
coef1 = slider(0, 300, initial = 60), 
coef2 = slider(0, 200, initial = 160), 
coef3 = slider(0, 200, initial = 0)
)
