#' Parse zone data by identifying line of ='s, which seperates column labels from the associated data

#' @inheritParams identify_headers
#' @param zone zone identifier returned by \code{\link{list_zones}}

#' @export

parse_zone <- function(txt, zone) {
  
  stopifnot(!missing(zone))
  
  if(grepl("Residual", zone)) {
    pattern <- paste(zone, "$", sep = "")
  } else {
    pattern <- paste(zone, "\\s+\\(", sep = "")
  }
  
  # Exclude text before start of specified zone
  txt <- txt[grep(pattern, txt):length(txt)]
  
  hrule.line <- identify_breaks(txt, break.char = "=")[1]
  blank.lines <- which(txt == "")
  
  # Lines containing column headers
  head.lines <- seq(
    blank.lines[findInterval(hrule.line, blank.lines)] + 1,
    hrule.line - 1, 1)
  
  # Lines containing data rows
  data.lines <- seq(hrule.line + 1, 
    blank.lines[findInterval(hrule.line, blank.lines) + 1] - 1, 1)
  
  # Split data into distinc columns
  df.data <- trim_space(txt[data.lines])
  df.data <- sapply(df.data, function(x) strsplit(x, split = "\\s+"))
  df.data <- do.call("rbind", df.data)
  
  # Split and collapse column headers into vector
  df.labels <- sapply(txt[head.lines], function(x) strsplit(x, split = "\\s+"))
  df.labels <- do.call("rbind", df.labels)
  df.labels <- apply(df.labels, 2, function(x) paste(x, collapse = ""))

  # Clean up df names
  rownames(df.data) <- 1:nrow(df.data)
  colnames(df.data) <- df.labels
  
  # Add zone identifier
  df.data <- data.frame(Zone = zone, df.data)
  
  return(df.data)
}