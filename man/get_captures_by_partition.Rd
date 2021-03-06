% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_captures_by_partition.R
\name{get_captures_by_partition}
\alias{get_captures_by_partition}
\title{Get captures within each piece-specific partition}
\usage{
get_captures_by_partition(game, partitionpiece, capturingpiece = NULL)
}
\arguments{
\item{game}{a full game returned by get_boards()}

\item{partitionpiece}{the piece by which the moves in the game should be partitioned}

\item{capturingpiece}{optional, a specific piece that is making captures during each partition}
}
\description{
Partitions the game based on the number of a specific type of piece remaining on the board, and outputs the captures that occurred during that partition.
}
