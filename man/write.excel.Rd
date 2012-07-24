\name{write.excel}
\alias{write.excel}
\title{Copies data to clipboard, ready for pasting into Excel.}
\usage{
  write.excel(tab, row.names = !is.data.frame(tab),
    col.names = is.data.frame(tab), ...)
}
\arguments{
  \item{tab}{A data.frame, array or other object that can
  be coerced to tabular format}

  \item{row.names}{Character vector of optional names for
  the variables.  Passed to \code{\link{write.table}}}

  \item{col.names}{Character vector of optional names for
  the variables. Passed to \code{\link{write.table}}}

  \item{...}{Passed to \code{\link{write.table}}}
}
\description{
  Copies data to clipboard, ready for pasting into Excel.
  Used for its side-effects.
}
\examples{
write.excel(mtcars)
write.excel(islands)
}
\seealso{
  \code{\link{write.excel}} and the base functions
  \code{\link{write.table}}, \code{\link{writeClipboard}}

  Other Functions.that.copy.paste:
  \code{\link{read.excel}}, \code{\link{write.ppt}}
}

