#' Copies data to clipboard, ready for pasting into Excel.
#'
#' Copies data to clipboard, ready for pasting into Excel.  Used for its side-effects. 
#' 
#' @param tab A data.frame, array or other object that can be coerced to tabular format
#' @param row.names Character vector of optional names for the variables.  Passed to \code{\link{write.table}}
#' @param col.names Character vector of optional names for the variables. Passed to \code{\link{write.table}}
#' @param ... Passed to \code{\link{write.table}}
#' @family Functions that copy/paste
#' @seealso \code{\link{write.excel}} and the base functions \code{\link{write.table}}, \code{\link{writeClipboard}}
#' @export
#' @examples 
#' write.excel(mtcars)
#' write.excel(islands)
write.excel <- function(tab, row.names=!is.data.frame(tab), col.names=is.data.frame(tab), ...){
  write.table(tab, "clipboard", sep="\t", row.names=row.names, col.names=col.names, ...)
  invisible(NULL)
}

#' Copies data from clipboard, e.g. when pasting from Excel.
#'
#' Copies data from clipboard, e.g. when pasting from Excel.
#' 
#' @param header If TRUE, reads data with header row
#' @param ... Passed to \code{\link{read.table}}
#' @family Functions that copy/paste
#' @export
#' @seealso \code{\link{read.excel}} and the base functions \code{\link{read.table}}, \code{\link{readClipboard}}
read.excel <- function(header=TRUE, ...){
  clip <- file(description="clipboard", open="r")
  on.exit(close(clip))
  read.table(file=clip, sep="\t", header=header, ...)
}



#' Copies data to clipboard, ready for pasting into PowerPoint or Word.
#'
#' Puts a copy of an object into the clipboard.  This is a wrapper around \code{\link{write.excel}} 
#'
#' @param x A data.frame, array or other object that can be coerced to tabular format
#' @inheritParams write.excel 
#' @family Functions that copy/paste
#' @export 
#' @examples 
#' write.ppt(mtcars)
#' write.ppt(islands)
write.ppt <- function (x, row.names=FALSE, col.names = if(is.data.frame(x)) names(x) else FALSE, ...) {
  write.table(x, "clipboard", sep = "\t", row.names = row.names, col.names=col.names,  ...)
  invisible(NULL)
}




#' Wrapper around read.table and textConnection.
#'
#' Reads data from textConnection and processes it with read.table to output a data.frame.
#' 
#' @param x The text to read
#' @param header If TRUE, reads data with header row
#' @param ... Passed to \code{\link{read.table}}
#' @return data.frame
#' @export
#' @examples
#' read.tc("a b c\n1 2 3\n4 5 6")
read.tc <- function(x, header=TRUE, ...){
  zz <- textConnection(x)
  on.exit(close(zz))
  read.table(file=zz, header=header, ...)
}

#' Prints ggplot graphics in grid layout.
#' 
#' Takes a list of ggplot objects and prints these using a grid layout supplied by \code{\link[grid]{grid.layout}}. In this way, a list of plots can be laid out in complex ways on the screen.
#' 
#' The plotsin \code{plotList} are printed sequentially to the viewports defined by \code{layout}, in order of rows followed by columns.  If an element of \code{plotList} is \code{NULL}, then that plot is empty.
#' @param plotList A \code{list} of \code{ggplot} objects
#' @param layout A layout provided by \code{\link[grid]{grid.layout}}. Defaults to a vertical column.
#' @param colwise If TRUE, the plots are arranged column-wise
#' @param rowwise If TRUE, the plots are arranged row-wise
#' @export
#' @examples 
#' require(ggplot2)
#' require(grid)
#' p <- ggplot(mtcars, aes(factor(cyl)))
#' pList <- list(
#'     p + geom_bar(),
#'     NULL,
#'     p + geom_bar() + coord_flip(),
#'     p + geom_bar(fill="white", colour="darkgreen")
#' )
#' 
#' gglayout(pList)
#' gglayout(pList, layout=grid.layout(4, 1))
#' gglayout(pList, layout=grid.layout(1, 4))
#' gglayout(pList, layout=grid.layout(2, 2), rowwise=TRUE)
gglayout <- function(plotList, layout=grid.layout(length(plotList), 1), colwise=TRUE, rowwise=!colwise){
  vplayout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
  grid.newpage()
  pushViewport(viewport(layout=layout))

  xx <- layout$ncol
  yy <- layout$nrow
  p <- 1
  
  for(i in seq_len(xx)){
    for(j in seq_len(yy)){
      if(rowwise) vp <- vplayout(i,j) else vp <- vplayout(j,i)
      if(p <= length(plotList)) print(plotList[[p]], vp=vp)
      p <- p + 1
    }
  }
  return(invisible(NULL))
}
