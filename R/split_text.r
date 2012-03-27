#' Split raw text into subject specific sections

#' @inheritParams identify_headers
#' @param split.lines numeric vector indicating where the zone file should be split. Typically these are header lines identified by \code{\link{identify_headers}}.
#' @param include.lines logical, should the split lines be included in the output?

#' @export

#' @return A list containing relevant lines of zone file for each subject.
split_text <- function(txt, split.lines, include.lines = F) {

  stopifnot(!missing(split.lines))

  end.lines <- c(tail(split.lines, -1) - 1, length(txt))

  if(!include.lines) {
    split.lines <- split.lines + 1
  }

  split.txt <- apply(data.frame(start = split.lines, end = end.lines), 1, 
    function(x) txt[x[1]:x[2]])

  return(split.txt)
}