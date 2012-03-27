\name{identify_headers}
\alias{identify_headers}
\title{Identify headers denoting new subject}
\usage{
  identify_headers(txt, header = "Activity Zone Analysis",
    ...)
}
\arguments{
  \item{txt}{character vector containing each line of the
  raw .zone file}

  \item{header}{pattern used by \code{\link{grep}} to
  identify lines containing headers}

  \item{...}{additional arguments to pass on to
  \code{\link{grep}}}
}
\value{
  numeric vector giving line numbers containing the
  provided \code{header} pattern.
}
\description{
  Identify headers denoting new subject
}

