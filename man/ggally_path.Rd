\name{ggally_path}
\alias{ggally_path}
\title{Connects the dots between points, using path for special ordering}
\usage{
ggally_path(data, mapping, ...)
}
\arguments{
  \item{data}{data set using}

  \item{mapping}{aesthetics being used}

  \item{...}{other arguments are sent to geom_point}
}
\description{
  Make a line plot plot with a given data set
}
\examples{
df <- data.frame(x = sin(2 * pi * seq(0, 1, 0.01)), y = cos(2 * pi * 
    seq(0, 1, 0.01)))
ggally_path(df, mapping = ggplot2::aes(x = x, y = y))
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}
