#' @include ggmatrix.R
NULL

ggplot2_set_last_plot <- utils::getFromNamespace("set_last_plot", "ggplot2")

#' Print \code{\link{ggmatrix}} object
#'
#' Print method taken from \code{ggplot2:::print.ggplot} and altered for a \code{\link{ggmatrix}} object
#'
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... arguments passed onto \code{\link{ggmatrix_gtable}}
#' @author Barret Schloerke
#' @import utils
#' @importFrom grid grid.newpage grid.draw seekViewport pushViewport upViewport
# ' @export
#' @name print.ggmatrix
#' @examples
#' data(tips)
#' pMat <- ggpairs(tips, c(1, 3, 2), mapping = ggplot2::aes(color = sex))
#' pMat # calls print(pMat), which calls print.ggmatrix(pMat)
method(print, ggmatrix) <- function(x, newpage = TRUE, vp = NULL, ...) {
  if (newpage) {
    grid.newpage()
  }
  grDevices::recordGraphics(
    requireNamespace("GGally", quietly = TRUE),
    list(),
    getNamespace("GGally")
  )
  gtable <- ggmatrix_gtable(x, ...)

  # must be done after gtable, as gtable calls many ggplot2::print.ggplot methods
  ggplot2_set_last_plot(x)

  if (is.null(vp)) {
    grid.draw(gtable)
  } else {
    if (is.character(vp)) {
      seekViewport(vp)
    } else {
      pushViewport(vp)
    }
    grid.draw(gtable)
    upViewport()
  }
  invisible(data)
}


#' Is Blank Plot?
#' Find out if the plot equals a blank plot
#'
#' @keywords internal
#' @examples
#' GGally:::is_blank_plot(ggally_blank())
#' GGally:::is_blank_plot(ggally_points(mtcars, ggplot2::aes(disp, hp)))
#'
is_blank_plot <- function(p) {
  is.null(p) || identical(p, "blank") || inherits(p, "ggmatrix_blank")
}
