#' Convert MedAssociates time formats to numeric seconds
#'
#' @param x Character vector of times in the MedAssociates format (e.g., 0000:00.00)
#' @param pattern regular expression pattern used to match time format. A commonly used format is set by default.
#' @param use an optional character string indicating how values not matching the time format pattern should be handled. This must be (an abbreviation) of one of the strings "stop", "preserve" or "ignore". 
#'
#' @return numeric vector of time in seconds.
#'
#' @export

convert_time <- function(x, pattern, use = "stop") {

  if(missing(pattern)) {
    pattern <- "\\d{4}:\\d{2}\\.\\d{2}"
  }

  use <- pmatch(use, c("stop", "preserve", "ignore"))

  matches <- grepl(pattern, x)

  if(use == "stop" & !any(matches)) {
    stop("x doesn't match time pattern.")
  } else {
    use == "ignore"
  }

  secs <- calc_seconds(x[matches])

  if(use == "ignore") {
    return(secs)
  } else if (use == "preserve") {
    out <- rep(NA, length(x))
    out[which(matches)] <- secs
    out[which(!matches)] <- x[which(!matches)]
    return(out)
  }
}

#' Calculate seconds from character strings in specified formats

#' @inheritParams convert_time

calc_seconds <- function(x) {
  x <- do.call("rbind", strsplit(x, split = ":"))
  x <- apply(x, 2, as.numeric)
  x <- (x[, 1] * 60) + x[, 2]
  return(x)
}