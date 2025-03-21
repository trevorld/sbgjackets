% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/looney_pyramids.r
\name{sbgj_looney_pyramids_all}
\alias{sbgj_looney_pyramids_all}
\alias{sbgj_looney_pyramids}
\alias{sbgj_homeworlds}
\alias{sbgj_ice_duo}
\alias{sbgj_jinxx}
\alias{sbgj_martian_chess}
\alias{sbgj_nomids}
\title{Create SBG Jacket for Looney Pyramids}
\usage{
sbgj_looney_pyramids_all(
  output = NULL,
  ...,
  paper = c("letter", "a4"),
  instructions = TRUE
)

sbgj_looney_pyramids(
  output = NULL,
  ...,
  paper = c("letter", "a4"),
  instructions = FALSE
)

sbgj_homeworlds(
  output = NULL,
  ...,
  paper = c("letter", "a4"),
  instructions = FALSE
)

sbgj_ice_duo(
  output = NULL,
  ...,
  paper = c("letter", "a4"),
  instructions = FALSE
)

sbgj_jinxx(output = NULL, ..., paper = c("letter", "a4"), instructions = FALSE)

sbgj_martian_chess(
  output = NULL,
  ...,
  silver = FALSE,
  paper = c("letter", "a4"),
  instructions = FALSE
)

sbgj_nomids(
  output = NULL,
  ...,
  paper = c("letter", "a4"),
  instructions = FALSE,
  custom = FALSE
)
}
\arguments{
\item{output}{Output file name.  Defaults to \code{tempfile(fileext = ".pdf")}.}

\item{...}{Should be left empty.}

\item{paper}{Paper size.  Either "letter", "a4", or "special".}

\item{instructions}{If \code{TRUE} then prepend instructions on how to make the jacket to the beginning of the pdf}

\item{silver}{If \code{TRUE} make jacket for silver Martian Chess.}

\item{custom}{If \code{TRUE} use custom back imagery.}
}
\value{
The output file name invisibly.  As a side effect creates a pdf file.
}
\description{
\code{sbgj_looney_pyramids()} creates a small box game jacket for \href{https://www.looneylabs.com/pyramids-home}{Looney Pyramids}.
\code{sbgj_homeworlds()} creates a small box game jacket for \href{https://www.looneylabs.com/games/homeworlds}{Homeworlds}.
\code{sbgj_ice_duo()} creates a small box game jacket for \href{https://www.looneylabs.com/games/ice-duo}{Ice Duo}.
\code{sbgj_jinxx()} creates a small box game jacket for \href{https://www.looneylabs.com/games/jinxx}{Jinxx}.
\code{sbgj_martian_chess()} creates a small box game jacket for \href{https://www.looneylabs.com/games/martian-chess}{Martian Chess}.
\code{sbgj_nomids()} creates a small box game jacket for \href{https://www.looneylabs.com/games/nomids}{Nomids}.
\code{sbgj_looney_pyramids_all()} creates all of those into a single pdf file.
}
\details{
Note that these print-and-play small box game jackets are for \strong{Personal Use Only}.
These jackets use images by Looney Labs.  These jackets are \strong{not} for distribution.
For more information see the Looney Labs FAQ: \url{https://faq.looneylabs.com/non-gameplay-questions/working-with-looney-labs#1774}.
}
