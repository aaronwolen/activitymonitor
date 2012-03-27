\name{parse_zone}
\alias{parse_zone}
\title{Parse zone data by identifying line of ='s, which seperates column labels from the associated data}
\usage{
  parse_zone(txt, zone)
}
\arguments{
  \item{zone}{zone identifier returned by
  \code{\link{list_zones}}}

  \item{txt}{character vector containing each line of the
  raw .zone file}
}
\description{
  Parse zone data by identifying line of ='s, which
  seperates column labels from the associated data
}

