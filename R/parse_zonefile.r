#' Read in and parse zone file

#' @param file Zone file

#' @export
parse_zonefile <- function(file) {
  
  txt <- readLines(file, encoding = "UTF-8")

  # Split into list of multiple subjects
  split.txt  <- split_text(raw, split.lines = identify_headers(txt))

  # Extract meta data for each subject
  meta.data <- lapply(split.txt, function(x) parse_meta(x))

  # For each subject identify zones and extract corresponding data
  zone.data <- lapply(split.txt, function(x) 
    sapply(list_zones(x), simplify = F, function(z) parse_zone(x, z)))

  # For each subject create single data frame with all zones
  zone.data <- lapply(zone.data, 
    function(x) data.frame(do.call("rbind", x), row.names = NULL))

  # Merge meta and zone data
  parsed <- do.call("rbind", sapply(1:length(split.text), simplify = F,
    function(x) merge(meta.data[[x]], zone.data[[x]])))
    
  return(parsed)
}