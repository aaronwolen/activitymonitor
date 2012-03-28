#' Read in and parse zone file

#' @param file Zone file
#' @param convert.times use \code{\link{convert_time}} to automatically identify MedAssociates time formats and convert values to numeric seconds

#' @export
parse_zonefile <- function(file, convert.times = TRUE) {
  
  txt <- readLines(file, encoding = "UTF-8")

  # Split into list of multiple subjects
  split.txt  <- split_text(txt, split.lines = identify_headers(txt))

  # Extract meta data for each subject
  meta.data <- lapply(split.txt, function(x) parse_meta(x))

  # For each subject identify zones and extract corresponding data
  zone.data <- lapply(split.txt, function(x) 
    sapply(list_zones(x), simplify = F, function(z) parse_zone(x, z)))

  # For each subject create single data frame with all zones
  zone.data <- lapply(zone.data, 
    function(x) data.frame(do.call("rbind", x), row.names = NULL))
  
  # Add Time.Block column
  zone.data <- lapply(zone.data, function(x) 
    data.frame(Time.Block = rep(1:table(x$Zone)[1], nlevels(factor(x$Zone))), x))

  # Merge meta and zone data
  parsed <- do.call("rbind", sapply(1:length(zone.data), simplify = F,
    function(x) merge(meta.data[[x]], zone.data[[x]])))
  
  if(convert.times) {
    parsed <- colwise(function(x) convert_time(x, use = "preserve"))(parsed)
  }
    
  return(parsed)
}