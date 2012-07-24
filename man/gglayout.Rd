\name{gglayout}
\alias{gglayout}
\title{Prints ggplot graphics in grid layout.}
\usage{
  gglayout(plotList,
    layout = grid.layout(1, length(plotList)),
    colwise = TRUE, rowwise = !colwise)
}
\arguments{
  \item{plotList}{A \code{list} of \code{ggplot} objects}

  \item{layout}{A layout provided by
  \code{\link[grid]{grid.layout}}}
}
\description{
  Takes a list of ggplot objects and prints these using a
  grid layout supplied by \code{\link[grid]{grid.layout}}.
  In this way, a list of plots can be laid out in complex
  ways on the screen.
}
\details{
  The plotsin \code{plotList} are printed sequentially to
  the viewports defined by \code{layout}, in order of rows
  followed by columns.  If an element of \code{plotList} is
  \code{NULL}, then that plot is empty.
}
\examples{
require(ggplot2)
require(grid)
p <- ggplot(mtcars, aes(factor(cyl)))
pList <- list(
    p + geom_bar(),
    NULL,
    p + geom_bar() + coord_flip(),
    p + geom_bar(fill="white", colour="darkgreen")
)

gglayout(pList)
gglayout(pList, layout=grid.layout(4, 1))
gglayout(pList, layout=grid.layout(1, 4))
gglayout(pList, layout=grid.layout(2, 2), rowwise=TRUE)
}

