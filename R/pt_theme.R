#' A custom ggplot theme
#'
#' This function applies a custom theme to a ggplot object. It is based on
#' theme_bw(), but with grid lines removed and facet boxes white with no border.
#'
#' @examples
#' # Create a basic ggplot
#' p <-  ggplot2::ggplot(mtcars, aes(mpg, hp)) + geom_point()
#'
#' # Apply the custom theme
#' p + theme_custom()

theme_pt <- function() {
  theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "white", colour = NA)
    )
}
