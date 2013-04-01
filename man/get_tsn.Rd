\name{get_tsn}
\alias{get_tsn}
\title{Get the TSN code for a search term.}
\usage{
  get_tsn(searchterm, searchtype = "sciname",
    verbose = TRUE, locally = FALSE, cn = NULL)
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

  \item{cn}{sqlite3 connection object}
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
get_tsn(searchterm="Quercus douglasii", searchtype="sciname", locally=FALSE)
get_tsn(searchterm="Quercus douglasii", searchtype="sciname", locally=TRUE, cn=conn)
get_tsn(searchterm=c("Chironomus riparius","Quercus douglasii"), searchtype="sciname", locally=TRUE, cn=conn)
splist <- c("annona cherimola", 'annona muricata', "quercus robur",
		"shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus", "Tamandua tetradactyla",
		"Tarsius syric", "Sylvilagus dicei", "Galeopterus variegatus")
get_tsn(splist,"sciname", locally=TRUE, cn=conn)
get_tsn(splist,"sciname")

# By common names
get_tsn(searchterm=c("polar bear", "ferret-badger", "american bullfrog"), searchtype = "comname", locally=TRUE, cn=conn)
}
}

