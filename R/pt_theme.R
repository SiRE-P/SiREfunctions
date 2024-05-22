#' A custom ggplot theme
#'
#' This function applies a custom theme to a ggplot object. It is based on
#' theme_bw(), but with grid lines removed and facet boxes white with no border.
#'
#' @examples
#' # Create a basic ggplot
#' p <-  ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point()
#'
#' # Apply the custom theme
#' p + theme_pt()

theme_pt <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}
