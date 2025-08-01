% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggpairs_add.R
\name{add_to_ggmatrix}
\alias{add_to_ggmatrix}
\title{Modify a \code{\link{ggmatrix}} object by adding an \pkg{ggplot2} object to all plots}
\usage{
add_to_ggmatrix(e1, e2, location = NULL, rows = NULL, cols = NULL)
}
\arguments{
\item{e1}{An object of class \code{\link{ggnostic}} or \code{ggplot}}

\item{e2}{A component to add to \code{e1}}

\item{location}{\describe{
\item{\code{"all"}, \code{TRUE}}{All row and col combinations}
\item{\code{"none"}}{No row and column combinations}
\item{\code{"upper"}}{Locations where the column value is higher than the row value}
\item{\code{"lower"}}{Locations where the row value is higher than the column value}
\item{\code{"diag"}}{Locations where the column value is equal to the row value}
\item{\code{matrix} or \code{data.frame}}{
\code{matrix} values will be converted into \code{data.frame}s.
\itemize{
\item A \code{data.frame} with the exact column names \code{c("row", "col")}
\item A \code{data.frame} with the number of rows and columns matching the plot matrix object provided.  Each cell will be tested for a "truthy" value to determine if the location should be kept.
}
}
}}

\item{rows}{numeric vector of the rows to be used. Will be used with  \code{cols} if \code{location} is \code{NULL}}

\item{cols}{numeric vector of the cols to be used. Will be used with \code{rows} if \code{location} is \code{NULL}}
}
\description{
This operator allows you to add \pkg{ggplot2} objects to a \code{\link{ggmatrix}} object.
}
\details{
If the first object is an object of class \code{\link{ggmatrix}}, you can add
the following types of objects, and it will return a modified \pkg{ggplot2}
object.

\itemize{
\item \code{theme}: update plot theme
\item \code{scale}: replace current scale
\item \code{coord}: override current coordinate system
}

The \code{+} operator completely replaces elements
with elements from e2.

\code{add_to_ggmatrix} gives you more control to modify
only some subplots.  This function may be replaced and/or removed in the future. \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
}
\examples{
# small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive
data(tips)

pm <- ggpairs(tips[, 2:4], ggplot2::aes(color = sex))
## change to black and white theme
pm + ggplot2::theme_bw()
## change to linedraw theme
p_(pm + ggplot2::theme_linedraw())
## change to custom theme
p_(pm + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "lightblue")))
## add a list of information
extra <- list(ggplot2::theme_bw(), ggplot2::labs(caption = "My caption!"))
p_(pm + extra)

## modify scale
p_(pm + scale_fill_brewer(type = "qual"))
## only first row
p_(add_to_ggmatrix(pm, scale_fill_brewer(type = "qual"), rows = 1:2))
## only second col
p_(add_to_ggmatrix(pm, scale_fill_brewer(type = "qual"), cols = 2:3))
## only to upper triangle of plot matrix
p_(add_to_ggmatrix(
  pm,
  scale_fill_brewer(type = "qual"),
  location = "upper"
))
}
\seealso{
\code{\link{ggmatrix_location}}
}
