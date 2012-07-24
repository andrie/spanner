\name{write.ppt}
\alias{write.ppt}
\title{Copies data to clipboard, ready for pasting into PowerPoint or Word.}
\usage{
  write.ppt(x, row.names = FALSE, col.names = names(x),
  ...)
}
\arguments{
  \item{x}{A data.frame, array or other object that can be
  coerced to tabular format}

  \item{row.names}{Character vector of optional names for
  the variables.  Passed to \code{\link{write.table}}}

  \item{col.names}{Character vector of optional names for
  the variables. Passed to \code{\link{write.table}}}

  \item{...}{Passed to \code{\link{write.table}}}
}
\description{
  Puts a copy of an object into the clipboard.  This is a
  wrapper around \code{\link{write.excel}}
}
\examples{
write.ppt(mtcars)
write.ppt(islands)
}
\seealso{
  Other Functions.that.copy.paste:
  \code{\link{read.excel}}, \code{\link{write.excel}}
}

