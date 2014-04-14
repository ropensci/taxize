\name{gnr_resolve}
\alias{gnr_resolve}
\title{Resolve names using Global Names Resolver.}
\usage{
gnr_resolve(names, data_source_ids = NULL, resolve_once = FALSE,
  with_context = FALSE, stripauthority = FALSE, highestscore = TRUE,
  http = "get", callopts = list())
}
\arguments{
  \item{names}{character; taxonomic names to be resolved.}

  \item{data_source_ids}{character; IDs to specify what
  data source is searched. See
  \code{\link[taxize]{gnr_datasources}}.}

  \item{resolve_once}{logical; Find the first available
  match instead of matches across all data sources with all
  possible renderings of a name.  When \code{TRUE},
  response is rapid but incomplete.}

  \item{with_context}{logical; Reduce the likelihood of
  matches to taxonomic homonyms. When \code{TRUE} a common
  taxonomic context is calculated for all supplied names
  from matches in data sources that have classification
  tree paths. Names out of determined context are penalized
  during score calculation.}

  \item{stripauthority}{logical; If \code{TRUE}, gives back
  names with taxonomic authorities. If \code{FALSE}, strips
  author names.}

  \item{highestscore}{logical; Return those names with the
  highest score for each searched name?}

  \item{http}{The HTTP method to use, one of "get" or
  "post". Default="get".  Use http="post" with large
  queries. Queries with > 300 records use "post"
  automatically because "get" would fail}

  \item{callopts}{Curl debugging options to pass in
  httr::GET or POST}
}
\value{
A data.frame.
}
\description{
Uses the Global Names Index, see
\url{http://gni.globalnames.org/}.
}
\examples{
\dontrun{
gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"))
gnr_resolve(names = c("Asteraceae", "Plantae"))

# Using data source 12 (Encyclopedia of Life)
sources<- gnr_datasources()
sources
eol <- sources$id[sources$title == 'EOL']
gnr_resolve(names=c("Helianthos annuus","Homo sapians"), data_source_ids=eol)
}
}
\author{
Scott Chamberlain {myrmecocystus@gmail.com}
}
\seealso{
\code{\link[taxize]{gnr_datasources}}
}
\keyword{names}
\keyword{resolve}
\keyword{taxonomy}

