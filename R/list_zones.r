#' List zone names contained in zone file

#' @inheritParams identify_headers

#' @export

list_zones <- function(txt) {
  
  # Trim txt to include only zone data area
  txt <- txt[(grep("Zones Defined", txt) + 1):(grep("Zone Totals", txt) - 1)]

  # Identify and clean up zone names
  zones <- txt[grep("^Zone|^Residual", txt)]
  zones <- gsub("(^\\w+\\s+(\\d{1}|\\w+)).*", "\\1", zones)
  
  return(unique(zones))
}
