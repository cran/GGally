% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggscatmat.R
\name{uppertriangle}
\alias{uppertriangle}
\title{Rearrange dataset as the preparation of \code{\link{ggscatmat}} function}
\usage{
uppertriangle(
  data,
  columns = 1:ncol(data),
  color = NULL,
  corMethod = "pearson"
)
}
\arguments{
\item{data}{a data matrix. Should contain numerical (continuous) data.}

\item{columns}{an option to choose the column to be used in the raw dataset. Defaults to \code{1:ncol(data)}}

\item{color}{an option to choose a factor variable to be grouped with. Defaults to \code{(NULL)}}

\item{corMethod}{method argument supplied to \code{\link[stats]{cor}}}
}
\description{
Function for making the dataset used to plot the uppertriangle plots.
}
\examples{
data(flea)
head(uppertriangle(flea, columns = 2:4))
head(uppertriangle(flea))
head(uppertriangle(flea, color = "species"))
}
\author{
Mengjia Ni, Di Cook
}
