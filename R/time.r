#' Calculate seconds from MedAssociates time formats

#' @param x Character vector of times in the MedAssociates format (e.g., 0000:00.00)

#' @return numeric vector of time in seconds.

#' @export

calc_seconds <- function(x) {
  
  # Check for expected format
  if(any(!grepl("\\d{4}:\\d{2}\\.\\d{2}", x))) {
    stop("Times are not in expected format.")
  }
  
  x <- do.call("rbind", strsplit(x, split = ":"))
  x <- apply(x, 2, as.numeric)
  return((x[, 1] * 60) + x[, 2])
}