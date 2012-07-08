\name{ggally_lines}
\alias{ggally_lines}
\title{Connects the dots between points}
\usage{
ggally_lines(data, mapping, ...)
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
df <- data.frame(x = 2 * pi * seq(0, 1, 0.01), y = cos(2 * pi * seq(0, 
    1, 0.01)))
ggally_lines(df, mapping = ggplot2::aes(x = x, y = y))
}
\author{
  Barret Schloerke \email{schloerke@gmail.com}
}
\keyword{hplot}
