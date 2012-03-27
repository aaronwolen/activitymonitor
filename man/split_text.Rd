\name{split_text}
\alias{split_text}
\title{Split raw text into subject specific sections}
\usage{
  split_text(txt, split.lines, include.lines = F)
}
\arguments{
  \item{split.lines}{numeric vector indicating where the
  zone file should be split. Typically these are header
  lines identified by \code{\link{identify_headers}}.}

  \item{include.lines}{logical, should the split lines be
  included in the output?}

  \item{txt}{character vector containing each line of the
  raw .zone file}
}
\description{
  Split raw text into subject specific sections
}

