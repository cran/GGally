% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggnet2.R
\name{ggnet2}
\alias{ggnet2}
\title{Network plot}
\usage{
ggnet2(
  net,
  mode = "fruchtermanreingold",
  layout.par = NULL,
  layout.exp = 0,
  alpha = 1,
  color = "grey75",
  shape = 19,
  size = 9,
  max_size = 9,
  na.rm = NA,
  palette = NULL,
  alpha.palette = NULL,
  alpha.legend = NA,
  color.palette = palette,
  color.legend = NA,
  shape.palette = NULL,
  shape.legend = NA,
  size.palette = NULL,
  size.legend = NA,
  size.zero = FALSE,
  size.cut = FALSE,
  size.min = NA,
  size.max = NA,
  label = FALSE,
  label.alpha = 1,
  label.color = "black",
  label.size = max_size/2,
  label.trim = FALSE,
  node.alpha = alpha,
  node.color = color,
  node.label = label,
  node.shape = shape,
  node.size = size,
  edge.alpha = 1,
  edge.color = "grey50",
  edge.lty = "solid",
  edge.size = 0.25,
  edge.label = NULL,
  edge.label.alpha = 1,
  edge.label.color = label.color,
  edge.label.fill = "white",
  edge.label.size = max_size/2,
  arrow.size = 0,
  arrow.gap = 0,
  arrow.type = "closed",
  legend.size = 9,
  legend.position = "right",
  ...
)
}
\arguments{
\item{net}{an object of class \code{\link[network]{network}}, or any object
that can be coerced to this class, such as an adjacency or incidence matrix,
or an edge list: see \link[network]{edgeset.constructors} and
\link[network]{network} for details. If the object is of class
\link[igraph:aaa-igraph-package]{igraph} and the
\link[intergraph:intergraph-package]{intergraph} package is installed,
it will be used to convert the object: see
\code{\link[intergraph]{asNetwork}} for details.}

\item{mode}{a placement method from those provided in the
\code{\link[sna]{sna}} package: see \link[sna:gplot.layout]{gplot.layout} for
details. Also accepts the names of two numeric vertex attributes of
\code{net}, or a matrix of numeric coordinates, in which case the first two
columns of the matrix are used.
Defaults to the Fruchterman-Reingold force-directed algorithm.}

\item{layout.par}{options to be passed to the placement method, as listed in
\link[sna]{gplot.layout}.
Defaults to \code{NULL}.}

\item{layout.exp}{a multiplier to expand the horizontal axis if node labels
get clipped: see \link[scales]{expand_range} for details.
Defaults to \code{0} (no expansion).}

\item{alpha}{the level of transparency of the edges and nodes, which might be
a single value, a vertex attribute, or a vector of values.
Also accepts \code{"mode"} on bipartite networks (see 'Details').
Defaults to \code{1} (no transparency).}

\item{color}{the color of the nodes, which might be a single value, a vertex
attribute, or a vector of values.
Also accepts \code{"mode"} on bipartite networks (see 'Details').
Defaults to \code{grey75}.}

\item{shape}{the shape of the nodes, which might be a single value, a vertex
attribute, or a vector of values.
Also accepts \code{"mode"} on bipartite networks (see 'Details').
Defaults to \code{19} (solid circle).}

\item{size}{the size of the nodes, in points, which might be a single value,
a vertex attribute, or a vector of values. Also accepts \code{"indegree"},
\code{"outdegree"}, \code{"degree"} or \code{"freeman"} to size the nodes by
their unweighted degree centrality (\code{"degree"} and \code{"freeman"} are
equivalent): see \code{\link[sna]{degree}} for details. All node sizes must
be strictly positive.
Also accepts \code{"mode"} on bipartite networks (see 'Details').
Defaults to \code{9}.}

\item{max_size}{the \emph{maximum} size of the node when \code{size} produces
nodes of different sizes, in points.
Defaults to \code{9}.}

\item{na.rm}{whether to subset the network to nodes that are \emph{not}
missing a given vertex attribute. If set to any vertex attribute of
\code{net}, the nodes for which this attribute is \code{NA} will be removed.
Defaults to \code{NA} (does nothing).}

\item{palette}{the palette to color the nodes, when \code{color} is not a
color value or a vector of color values. Accepts named vectors of color
values, or if \link[RColorBrewer:ColorBrewer]{RColorBrewer} is installed, any
ColorBrewer palette name: see \code{\link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal()}} and
\url{https://colorbrewer2.org/} for details.
Defaults to \code{NULL}, which will create an array of grayscale color values
if \code{color} is not a color value or a vector of color values.}

\item{alpha.palette}{the palette to control the transparency levels of the
nodes set by \code{alpha} when the levels are not numeric values.
Defaults to \code{NULL}, which will create an array of alpha transparency
values if \code{alpha} is not a numeric value or a vector of numeric values.}

\item{alpha.legend}{the name to assign to the legend created by
\code{alpha} when its levels are not numeric values.
Defaults to \code{NA} (no name).}

\item{color.palette}{see \code{palette}}

\item{color.legend}{the name to assign to the legend created by
\code{palette}.
Defaults to \code{NA} (no name).}

\item{shape.palette}{the palette to control the shapes of the nodes set by
\code{shape} when the shapes are not numeric values.
Defaults to \code{NULL}, which will create an array of shape values if
\code{shape} is not a numeric value or a vector of numeric values.}

\item{shape.legend}{the name to assign to the legend created by
\code{shape} when its levels are not numeric values.
Defaults to \code{NA} (no name).}

\item{size.palette}{the palette to control the sizes of the nodes set by
\code{size} when the sizes are not numeric values.}

\item{size.legend}{the name to assign to the legend created by
\code{size}.
Defaults to \code{NA} (no name).}

\item{size.zero}{whether to accept zero-sized nodes based on the value(s) of
\code{size}.
Defaults to \code{FALSE}, which ensures that zero-sized nodes are still
shown in the plot and its size legend.}

\item{size.cut}{whether to cut the size of the nodes into a certain number of
quantiles. Accepts \code{TRUE}, which tries to cut the sizes into quartiles,
or any positive numeric value, which tries to cut the sizes into that many
quantiles. If the size of the nodes do not contain the specified number of
distinct quantiles, the largest possible number is used.
See \code{\link[stats]{quantile}} and \code{\link[base]{cut}} for details.
Defaults to \code{FALSE} (does nothing).}

\item{size.min}{whether to subset the network to nodes with a minimum size,
based on the values of \code{size}.
Defaults to \code{NA} (preserves all nodes).}

\item{size.max}{whether to subset the network to nodes with a maximum size,
based on the values of \code{size}.
Defaults to \code{NA} (preserves all nodes).}

\item{label}{whether to label the nodes. If set to \code{TRUE}, nodes are
labeled with their vertex names. If set to a vector that contains as many
elements as there are nodes in \code{net}, nodes are labeled with these. If
set to any other vector of values, the nodes are labeled only when their
vertex name matches one of these values.
Defaults to \code{FALSE} (no labels).}

\item{label.alpha}{the level of transparency of the node labels, as a
numeric value, a vector of numeric values, or as a vertex attribute
containing numeric values.
Defaults to \code{1} (no transparency).}

\item{label.color}{the color of the node labels, as a color value, a vector
of color values, or as a vertex attribute containing color values.
Defaults to \code{"black"}.}

\item{label.size}{the size of the node labels, in points, as a numeric value,
a vector of numeric values, or as a vertex attribute containing numeric
values.
Defaults to \code{max_size / 2} (half the maximum node size), which defaults
to \code{4.5}.}

\item{label.trim}{whether to apply some trimming to the node labels. Accepts
any function that can process a character vector, or a strictly positive
numeric value, in which case the labels are trimmed to a fixed-length
substring of that length: see \code{\link[base]{substr}} for details.
Defaults to \code{FALSE} (does nothing).}

\item{node.alpha}{see \code{alpha}}

\item{node.color}{see \code{color}}

\item{node.label}{see \code{label}}

\item{node.shape}{see \code{shape}}

\item{node.size}{see \code{size}}

\item{edge.alpha}{the level of transparency of the edges.
Defaults to the value of \code{alpha}, which defaults to \code{1}.}

\item{edge.color}{the color of the edges, as a color value, a vector of color
values, or as an edge attribute containing color values.
Defaults to \code{"grey50"}.}

\item{edge.lty}{the linetype of the edges, as a linetype value, a vector of
linetype values, or as an edge attribute containing linetype values.
Defaults to \code{"solid"}.}

\item{edge.size}{the size of the edges, in points, as a numeric value, a
vector of numeric values, or as an edge attribute containing numeric values.
All edge sizes must be strictly positive.
Defaults to \code{0.25}.}

\item{edge.label}{the labels to plot at the middle of the edges, as a single
value, a vector of values, or as an edge attribute.
Defaults to \code{NULL} (no edge labels).}

\item{edge.label.alpha}{the level of transparency of the edge labels, as a
numeric value, a vector of numeric values, or as an edge attribute
containing numeric values.
Defaults to \code{1} (no transparency).}

\item{edge.label.color}{the color of the edge labels, as a color value, a
vector of color values, or as an edge attribute containing color values.
Defaults to \code{label.color}, which defaults to \code{"black"}.}

\item{edge.label.fill}{the background color of the edge labels.
Defaults to \code{"white"}.}

\item{edge.label.size}{the size of the edge labels, in points, as a numeric
value, a vector of numeric values, or as an edge attribute containing numeric
values. All edge label sizes must be strictly positive.
Defaults to \code{max_size / 2} (half the maximum node size), which defaults
to \code{4.5}.}

\item{arrow.size}{the size of the arrows for directed network edges, in
points. See \code{\link[grid]{arrow}} for details.
Defaults to \code{0} (no arrows).}

\item{arrow.gap}{a setting aimed at improving the display of edge arrows by
plotting slightly shorter edges. Accepts any value between \code{0} and
\code{1}, where a value of \code{0.05} will generally achieve good results
when the size of the nodes is reasonably small.
Defaults to \code{0} (no shortening).}

\item{arrow.type}{the type of the arrows for directed network edges. See
\code{\link[grid]{arrow}} for details.
Defaults to \code{"closed"}.}

\item{legend.size}{the size of the legend symbols and text, in points.
Defaults to \code{9}.}

\item{legend.position}{the location of the plot legend(s). Accepts all
\code{legend.position} values supported by \code{\link[ggplot2]{theme}}.
Defaults to \code{"right"}.}

\item{...}{other arguments passed to the \code{geom_text} object that sets
the node labels: see \code{\link[ggplot2]{geom_text}} for details.}
}
\description{
Function for plotting network objects using \pkg{ggplot2}, with additional control
over graphical parameters that are not supported by the \code{\link{ggnet}}
function. Please visit \url{https://github.com/briatte/ggnet} for the latest
version of ggnet2, and \url{https://briatte.github.io/ggnet/} for a vignette
that contains many examples and explanations.
}
\details{
The degree centrality measures that can be produced through the
\code{size} argument will take the directedness of the network into account,
but will be unweighted. To compute weighted network measures, see the
\code{tnet} package by Tore Opsahl (\code{help("tnet", package = "tnet")}).

The nodes of bipartite networks can be mapped to their mode by passing the
\code{"mode"} argument to any of \code{alpha}, \code{color}, \code{shape} and
\code{size}, in which case the nodes of the primary mode will be mapped as
\code{"actor"}, and the nodes of the secondary mode will be mapped as
\code{"event"}.
}
\examples{
# Small function to display plots only if it's interactive
p_ <- GGally::print_if_interactive

library(network)

# random adjacency matrix
x           <- 10
ndyads      <- x * (x - 1)
density     <- x / ndyads
m           <- matrix(0, nrow = x, ncol = x)
dimnames(m) <- list(letters[1:x], letters[1:x])
m[row(m) != col(m)] <- runif(ndyads) < density
m

# random undirected network
n <- network::network(m, directed = FALSE)
n

p_(ggnet2(n, label = TRUE))
p_(ggnet2(n, label = TRUE, shape = 15))
p_(ggnet2(n, label = TRUE, shape = 15, color = "black", label.color = "white"))

# add vertex attribute
x = network.vertex.names(n)
x = ifelse(x \%in\% c("a", "e", "i"), "vowel", "consonant")
n \%v\% "phono" = x

p_(ggnet2(n, color = "phono"))
p_(ggnet2(n, color = "phono", palette = c("vowel" = "gold", "consonant" = "grey")))
p_(ggnet2(n, shape = "phono", color = "phono"))

if (require(RColorBrewer)) {

  # random groups
  n \%v\% "group" <- sample(LETTERS[1:3], 10, replace = TRUE)

  p_(ggnet2(n, color = "group", palette = "Set2"))

}

# random weights
n \%e\% "weight" <- sample(1:3, network.edgecount(n), replace = TRUE)
p_(ggnet2(n, edge.size = "weight", edge.label = "weight"))

# edge arrows on a directed network
p_(ggnet2(network(m, directed = TRUE), arrow.gap = 0.05, arrow.size = 10))

# Padgett's Florentine wedding data
data(flo, package = "network")
flo

p_(ggnet2(flo, label = TRUE))
p_(ggnet2(flo, label = TRUE, label.trim = 4, vjust = -1, size = 3, color = 1))
p_(ggnet2(flo, label = TRUE, size = 12, color = "white"))
}
\seealso{
\code{\link{ggnet}} in this package,
\code{\link[sna]{gplot}} in the \code{\link[sna]{sna}} package, and
\code{\link[network]{plot.network}} in the \code{\link[network]{network}}
package
}
\author{
Moritz Marbach and Francois Briatte, with help from Heike Hofmann,
Pedro Jordano and Ming-Yu Liu
}
