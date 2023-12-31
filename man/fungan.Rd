% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/madantextnetwork.r
\name{fungan}
\alias{fungan}
\title{Persian Suffix Modification for 'Persian text here' Suffix}
\usage{
fungan(v)
}
\arguments{
\item{v}{A character vector of Persian words.}
}
\value{
Returns a character vector where each element corresponds to a word
from the input vector `v` with the 'Persian text here' suffix modified.
This results in a transformed vector where each word ending with
the specified suffix is altered. The length of the returned vector
matches the length of the input vector, and each word is modified
independently based on the presence of the specified suffix.
}
\description{
This function modifies Persian words ending with 'Persian text here' suffix.
}
\examples{
\dontrun{
  words <- c("Persian text here")
  modified_words <- fungan(words)
}
}
