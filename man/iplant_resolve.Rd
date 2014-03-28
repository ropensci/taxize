\name{iplant_resolve}
\alias{iplant_resolve}
\title{iplant resolution}
\usage{
iplant_resolve(query, retrieve = "all", callopts = list())
}
\arguments{
  \item{query}{Vector of names}

  \item{retrieve}{Specifies whether to retrieve all matches
  for the names submitted. One of 'best' (retrieves only
  the single best match for each name submitted) or 'all'
  (retrieves all matches)}
}
\description{
iplant resolution
}
\examples{
\dontrun{
iplant_resolve(query=c("Helianthus annuus", "Homo sapiens"))
}
}

