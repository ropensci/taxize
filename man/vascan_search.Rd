\name{vascan_search}
\alias{vascan_search}
\title{Search the CANADENSYS Vascan API}
\usage{
vascan_search(q, format = "json", raw = FALSE, callopts = list())
}
\arguments{
  \item{q}{(character) Can be a scientific name, a
  vernacular name or a VASCAN taxon identifier (e.g. 861)}

  \item{format}{(character) One of json (default) or xml.}

  \item{raw}{(logical) If TRUE, raw json or xml returned,
  if FALSE, parsed data returned.}

  \item{callopts}{(list) Further args passed on to
  htt::GET.}
}
\value{
json, xml or a data.frame.
}
\description{
For more information, see
\url{http://data.canadensys.net/vascan/search}.
}
\examples{
\dontrun{
vascan_search(q = "Helianthus annuus")
vascan_search(q = "Helianthus annuus", raw=TRUE)
}
}
\author{
Scott Chamberlain {myrmecocystus@gmail.com}
}
\references{
API docs \url{http://data.canadensys.net/vascan/api}. You
can also download bulk data
\url{http://data.canadensys.net/ipt/resource.do?r=vascan&request_locale=en}
}
\keyword{names}
\keyword{taxonomy}

