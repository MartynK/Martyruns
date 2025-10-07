
integral_up_to_x <- function(data, x_value, 
                             kernel = "biweight", 
                             ...) {
  density_estimate <- density(data, kernel = kernel, ...)
  cumsum(density_estimate$y[density_estimate$x <= x_value])/
    cumsum(density_estimate$y)
}

# Example usage:
integral_value <- integral_up_to_x(data_all$hr[resamp], 
                                   x_value = 150, kernel = "biweight", n = 2048, adjust = 0.6)
print(integral_value)
