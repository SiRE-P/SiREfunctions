% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pt_theme.R
\name{theme_pt}
\alias{theme_pt}
\title{A custom ggplot2 theme with optional grid lines}
\usage{
theme_pt(
  facet_text_size = 12,
  title_size = 12,
  major_grid = TRUE,
  minor_grid = FALSE
)
}
\arguments{
\item{facet_text_size}{Numeric, size of facet strip text. Default is 12.}

\item{title_size}{Numeric, size of plot and axis titles. Default is 12.}

\item{major_grid}{Logical, whether to show major grid lines. Default is FALSE.}

\item{minor_grid}{Logical, whether to show minor grid lines. Default is FALSE.}
}
\value{
A ggplot2 theme object.
}
\description{
Based on theme_bw() with a clean white background, optional grid lines, and simplified facet styling.
}
