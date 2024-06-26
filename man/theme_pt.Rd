% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pt_theme.R
\name{theme_pt}
\alias{theme_pt}
\title{A custom ggplot theme}
\usage{
theme_pt(facet_text_size = 12, title_size = 12, major_grid = FALSE)
}
\arguments{
\item{facet_text_size}{Numeric, size of the facet text. Default is 12.}

\item{title_size}{Numeric, size of the title text. Default is 12.}

\item{major_grid}{Logical, whether to show major grid lines. Default is FALSE.}
}
\value{
A ggplot2 theme.
}
\description{
This function applies a custom theme to a ggplot object. It is based on
theme_bw(), but with grid lines removed and facet boxes white with no border.
}
\examples{
# Create a basic ggplot
p <-  ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point()

# Apply the custom theme
p + theme_pt()
}
