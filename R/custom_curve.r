
custom_curve <- function( xmin = 0, 
                          xmax = 1,
                          x_in = seq(0,1, length.out = 10),
                          df = 3,
                          custom_coefs = c(100, 0.5, 1.3,0.7)) {
  
  dat_simp <- data.frame(
    x = seq(0,1,length.out=5))
  dat_simp$y <- (dat_simp$x * 0.1)^2
  
  mod <- lm( y ~ splines::ns(x,df=df), data = dat_simp)
  
  mod$coefficients <- custom_coefs
  
  return(predict(mod, newdata = data.frame( x = x_in)))
  
}

