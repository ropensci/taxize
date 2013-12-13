\name{vascan_search}
\alias{vascan_search}
\title{Search the CANADENSYS Vascan API.}
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
json, xml or a list.
}
\description{
For more information, see
\url{http://data.canadensys.net/vascan/search}.
}
\examples{
\dontrun{
vascan_search(q = "Helianthus annuus")
vascan_search(q = "Helianthus annuus", raw=TRUE)
vascan_search(q = c("Helianthus annuus", "Crataegus dodgei"), raw=TRUE)

# format type
## json
c <- vascan_search(q = "Helianthus annuus", format="json", raw=TRUE)
library(rjson)
fromJSON(c)

## xml
d <- vascan_search(q = "Helianthus annuus", format="xml", raw=TRUE)
library(XML)
xmlParse(d)

# lots of names, in this case 50
splist <- names_list(rank='species', size=50)
vascan_search(q = splist)
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

