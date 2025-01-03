% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_duration.R
\name{MXLDurationToType}
\alias{MXLDurationToType}
\title{MXL duration to (type-dot-triplet)}
\usage{
MXLDurationToType(mxlDuration, mxlDivision = 96)
}
\arguments{
\item{mxlDuration}{Positive integer, music XML "duration" expressed in
number of mxlDivision's.}

\item{mxlDivision}{Positive integer, musicXML "division" defining the
time resolution, i.e. the shortest possible note.
It is expressed as a fraction of a quarter note.
The value of 96 allows allows using 64th notes and their triplet/dotted versions.}
}
\value{
A list with the following fields:
\enumerate{
  \item type, numeric vector of types
  \item dot, logical vector of dot flags
  \item triplet, logical vector of triplet flags
  \item exact, logical, FALSE if the requested duration had to be trimmed
}
}
\description{
Convert a MusicXML duration into a (type-dot-triplet),
or a list of (type-dot-triplet) summing up to the requested duration.
The requested MusicXML duration may be trimmed if it cannot be expressed
as a multiple of the smallest available duration.
}
\examples{
MXLDurationToType(972)
}
