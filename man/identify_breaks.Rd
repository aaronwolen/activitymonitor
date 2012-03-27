\name{identify_breaks}
\alias{identify_breaks}
\title{Identify breaks denoted by lines begining with repeated hypens or other specified character that denotes section break}
\usage{
  identify_breaks(txt, break.char = "-")
}
\arguments{
  \item{break.char}{character or symbol used to construct
  horizontal breaks}

  \item{txt}{character vector containing each line of the
  raw .zone file}
}
\value{
  numeric vector giving line numbers containing section
  breaks
}
\description{
  Identify breaks denoted by lines begining with repeated
  hypens or other specified character that denotes section
  break
}

