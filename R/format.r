#' Trim white space before and after string

#' @inheritParams identify_headers

#' @export
trim_space <- function(txt) {
  gsub("^\\s+|\\s+$", "", txt)
}


#' Split character vectors and return data frame

#' @inheritParams identify_headers
#' @param split regular expression pattern used to delimit columns

#' @return Data frame
#' @export
split_cols <- function(txt, split) {
  
  if(missing(split)) {
    split <- "\\s+"
  }
  
  split.txt <- sapply(txt, function(x) strsplit(x, split))
  split.txt <- do.call("rbind", split.txt)
  split.txt <- apply(split.txt, 2, function(x) paste(x, collapse = "_"))
  return(split.txt)
}