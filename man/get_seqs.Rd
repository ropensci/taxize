\name{get_seqs}
\alias{get_seqs}
\title{Retrieve gene sequences from NCBI by accession number.}
\usage{
get_seqs(taxa, gene = "COI", seqrange = "1:3000", getrelated = FALSE,
  verbose = TRUE)
}
\arguments{
  \item{taxa}{Scientific name to search for (character).}

  \item{gene}{Gene (character) or genes (character vector)
  to search for.}

  \item{seqrange}{Sequence range, as e.g., "1:1000"
  (character).}

  \item{getrelated}{Logical, if TRUE, gets the longest
  sequences of a species in the same genus as the one
  searched for. If FALSE, get's nothing.}

  \item{verbose}{logical; If TRUE (default), informative
  messages printed.}
}
\description{
Function name changed to ncbi_getbyname.
}

