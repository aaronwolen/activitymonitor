#' Identify headers denoting new subject

#' @param txt character vector containing each line of the raw .zone file
#' @param header pattern used by \code{\link{grep}} to identify lines containing headers
#' @param ... additional arguments to pass on to \code{\link{grep}}

#' @return numeric vector giving line numbers containing the provided \code{header} pattern.

#' @export
identify_headers <- function(txt, header = "Activity Zone Analysis", ...) {
  grep(header, txt, ...)
}

#' Identify breaks denoted by lines begining with repeated hypens or other specified character that denotes section break

#' @inheritParams identify_headers
#' @param break.char character or symbol used to construct horizontal breaks

#' @return numeric vector giving line numbers containing section breaks

#' @export
identify_breaks <- function(txt, break.char = "-") {
  grep(paste("^\\", break.char, "{2,}", sep = ""), txt)
}