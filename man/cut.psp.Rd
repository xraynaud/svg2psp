% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut.psp.R
\name{cut.psp}
\alias{cut.psp}
\title{Cut a psp segments into smaller segments.}
\usage{
\method{cut}{psp}(datapsp, maxlength)
}
\arguments{
\item{datapsp}{PSP object to be cut}

\item{maxlength}{Maximum length of segments in the returned psp object}
}
\value{
Returns a new psp object with segments length of \code{maxlength} a max.
}
\description{
Cut a psp segments into smaller segments.
}