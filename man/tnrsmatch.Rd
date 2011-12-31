\name{tnrsmatch}
\alias{tnrsmatch}
\title{Match taxonomic names using the Taxonomic Name Resolution Service.}
\usage{
  tnrsmatch(retrieve = "best", taxnames = NA, output = NA,
    url = "http://ohmsford.iplantc.org/tnrsm-svc/matchNames?retrieve=")
}
\arguments{
  \item{Retrieve}{either 'best' or 'all' for returning the
  best matched or all names, respectively (character).}

  \item{Taxnames}{quoted taxonomic names to search in a
  vector (character).}

  \item{Output}{'all' for raw list output or 'names' for
  matched names and their match scores, plus plant family
  names (character).}

  \item{url}{The iPlant API url for the function (should be
  left to default).}
}
\value{
  Data.frame of results.
}
\description{
  Match taxonomic names using the Taxonomic Name Resolution
  Service.
}
\examples{
\dontrun{
mynames <- c("shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus", "rubus ulmifolius", "asclepias curassavica", "pistacia lentiscus")
tnrsmatch('best', mynames, 'names')
tnrsmatch(retrieve = 'all', taxnames = c('helianthus annuus', 'acacia', 'saltea'), output = 'names')
tnrsmatch(retrieve = 'all', taxnames = c('helianthus annuus', 'acacia', 'saltea'), output = 'all')
tnrsmatch(retrieve = 'best', taxnames = c('helianthus annuus', 'acacia', 'saltea'), output = 'names')
}
}

