% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nestorgames.r
\name{sbgj_nestorgames_all}
\alias{sbgj_nestorgames_all}
\alias{sbgj_shibumi}
\title{Create SBG Jacket for nestorgames products}
\usage{
sbgj_nestorgames_all(
  output = NULL,
  ...,
  paper = c("letter", "a4"),
  instructions = TRUE
)

sbgj_shibumi(
  output = NULL,
  ...,
  paper = c("letter", "a4"),
  instructions = FALSE
)
}
\arguments{
\item{output}{Output file name.  Defaults to \code{tempfile(fileext = ".pdf")}.}

\item{...}{Should be empty.}

\item{paper}{Paper size.  Either "letter", "a4", or "special".}

\item{instructions}{If \code{TRUE} then prepend instructions on how to make the jacket to the beginning of the pdf}
}
\value{
The output file name invisibly.  As a side effect creates a pdf file.
}
\description{
\code{sbgj_shibumi()} creates a small box game jacket for \href{https://cambolbro.com/games/shibumi/}{Shibumi}.
\code{sbgj_nestorgames_all()} creates all of those into a single pdf file.
}
\details{
Note that these print-and-play small box game jackets are for \strong{Personal Use Only}.
}
