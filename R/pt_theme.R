#' A custom ggplot2 theme with optional grid lines
#'
#' Based on theme_bw() with a clean white background, optional grid lines, and simplified facet styling.
#'
#' @param facet_text_size Numeric, size of facet strip text. Default is 12.
#' @param title_size Numeric, size of plot and axis titles. Default is 12.
#' @param major_grid Logical, whether to show major grid lines. Default is FALSE.
#' @param minor_grid Logical, whether to show minor grid lines. Default is FALSE.
#'
#' @return A ggplot2 theme object.
#' @export
theme_pt <- function(facet_text_size = 12, title_size = 12, major_grid = TRUE, minor_grid = FALSE) {
  grid_elements <- ggplot2::theme()

  if (!major_grid) grid_elements <- grid_elements + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  if (!minor_grid) grid_elements <- grid_elements + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  ggplot2::theme_bw() +
    grid_elements +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white", colour = NA),
      strip.text = ggplot2::element_text(size = facet_text_size),
      axis.title = ggplot2::element_text(size = title_size),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.background = ggplot2::element_rect(fill = NA)
    )
}
