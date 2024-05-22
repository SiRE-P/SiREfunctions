#' A custom ggplot theme
#'
#' This function applies a custom theme to a ggplot object. It is based on
#' theme_bw(), but with grid lines removed and facet boxes white with no border.
#'
#' @param facet_text_size Numeric, size of the facet text. Default is 12.
#' @param title_size Numeric, size of the title text. Default is 12.
#' @param major_grid Logical, whether to show major grid lines. Default is FALSE.
#'
#' @return A ggplot2 theme.
#'
#' @examples
#' # Create a basic ggplot
#' p <-  ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point()
#'
#' # Apply the custom theme
#' p + theme_pt()
#' @export
theme_pt <- function(facet_text_size = 12, title_size = 12, major_grid = FALSE) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white", colour = NA),
      strip.text.x = ggplot2::element_text(size = facet_text_size),
      strip.text.y = ggplot2::element_text(size = facet_text_size),
      axis.title=ggplot2::element_text(size = title_size),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.background = ggplot2::element_rect(fill= NA)
    )

  if(major_grid == FALSE){
    theme <- theme + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  }

  return(theme)
}
