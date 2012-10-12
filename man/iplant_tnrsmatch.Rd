\name{iplant_tnrsmatch}
\alias{iplant_tnrsmatch}
\title{Search Taxonomic Name Resolution Service}
\usage{
  iplant_tnrsmatch(retrieve = "best", taxnames = NA,
    output = NA, getpost = "get",
    url = "http://tnrs.iplantc.org/tnrsm-svc/matchNames",
    ..., curl = getCurlHandle())
}
\arguments{
  \item{retrieve}{either 'best' or 'all' for returning the
  best matched or all names, respectively (character).}

  \item{taxnames}{quoted taxonomic names to search in a
  vector (character).}

  \item{output}{'all' for raw list output or 'names' for
  matched names and their match scores, plus plant family
  names (character).}

  \item{getpost}{Use get or post for sending query. Post is
  sometimes needed for larger URL strings.}

  \item{url}{The iPlant API url for the function (should be
  left to default).}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\value{
  data.frame of results from TNRS plus the name submitted.
}
\description{
  Match taxonomic names using the Taxonomic Name Resolution
  Service (TNRS).  Returns score of the matched name, and
  whether it was accepted or not.
}
\examples{
\dontrun{
mynames <- c("shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus", "rubus ulmifolius", "asclepias curassavica", "pistacia lentiscus")
iplant_tnrsmatch('best', mynames, 'names', 'post')
iplant_tnrsmatch(retrieve = 'all', taxnames = c('helianthus annuus', 'acacia', 'gossypium'), output = 'names')
iplant_tnrsmatch(retrieve = 'all', taxnames = c('helianthus annuus', 'acacia', 'saltea'), output = 'all')
iplant_tnrsmatch(retrieve = 'best', taxnames = c('helianthus annuus', 'acacia', 'saltea'), output = 'names')
}
}

