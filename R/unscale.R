#' Unscale data function
#'
#' This function takes data on a standardized (mean = 0, sd = 1) scale and converts it back to the original scale.
#' This is useful for converting predictions from models that used standardized variables back to their original scale.
#'
#' @param x_scaled Vector, data on the standardized scale
#' @param x Vector, the original data that was standardized
#'
#' @return A vector of data in the unstandardized scale
#'
#'
unscale <- function(x_scaled, x){
  x_mean <- mean(x, na.rm = TRUE)
  x_sd <- sd(x, na.rm = TRUE)
  return((x_scaled * x_sd) + x_mean)
}
