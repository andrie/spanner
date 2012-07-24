\name{read.tc}
\alias{read.tc}
\title{Wrapper around read.table and textConnection.}
\usage{
  read.tc(x, header = TRUE, ...)
}
\arguments{
  \item{x}{The text to read}

  \item{header}{If TRUE, reads data with header row}

  \item{...}{Passed to \code{\link{read.table}}}
}
\value{
  data.frame
}
\description{
  Reads data from textConnection and processes it with
  read.table to output a data.frame.
}
\examples{
read.tc("a b c\\n1 2 3\\n4 5 6")
}

