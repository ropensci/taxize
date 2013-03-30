\name{get_tsn}
\alias{get_tsn}
\title{Get the TSN code for a search term.}
\usage{
  get_tsn(searchterm, searchtype = "sciname",
    verbose = TRUE, locally = FALSE)
}
\arguments{
  \item{searchterm}{A vector of common or scientific
  names.}

  \item{searchtype}{One of 'sciname', 'anymatch',
  'comnamebeg', 'comname', 'comnameend'.}

  \item{verbose}{should progress be printed?}

  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}
}
\value{
  A vector of taxonomic serial numbers (TSN). If a species
  is not found NA.  If more than one TSN is found the
  function asks for user input.  See functions in the
  \code{itis} function.
}
\description{
  \code{get_tsn} uses a variety of functions from the
  \code{itis} function.
}
\examples{
\dontrun{
get_tsn(searchterm="Quercus douglasii", searchtype="sciname")
get_tsn(searchterm="Chironomus riparius", searchtype="sciname")
get_tsn(c("Chironomus riparius","Quercus douglasii"), "sciname")
splist <- c("annona cherimola", 'annona muricata', "quercus robur",
		"shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus")
get_tsn(splist,"sciname")
}
}

