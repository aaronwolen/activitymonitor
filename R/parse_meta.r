#' Parse metadata contained between subject header and zone definition section

#' @inheritParams identify_headers

#' @export

parse_meta <- function(txt) {

  # Exclude text after meta data
  txt <- txt[1:(grep("Zones Defined", txt) - 1)]
  
  # Only keep lines with semicolons
  txt <- txt[grep("\\S:(\\s)+", txt)]
  
  meta <- sapply(txt, function(x) strsplit(x, split = ":(\\s)+"))
  meta <- data.frame(do.call("rbind", meta), 
    row.names = 1:length(meta), stringsAsFactors = F)
  names(meta) <- c("field", "value")
  
  meta$value <- trim_space(meta$value)

  # Transform into a wide data frame
  meta <- data.frame(matrix(meta$value, nrow = 1, 
    dimnames = list(1, meta$field)))
  
  return(meta)  
}