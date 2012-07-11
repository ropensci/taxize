\name{gnr}
\alias{gnr}
\title{Resolve names using Global Names Resolver.}
\usage{
  gnr(names, data_source_ids = NULL, output = "json",
    url = "http://resolver.globalnames.org/name_resolvers")
}
\arguments{
  \item{names}{Quoted taxonomic names to be resolved in a
  vector.}

  \item{data_source_ids}{Supply data source IDs to specify
  what data source is searched. See example below.}

  \item{output}{Output format for the results, one of json
  (default) or xml.}

  \item{url}{Base url for the API; leave as is.}
}
\value{
  json or xml output, your choice
}
\description{
  Uses the Global Names Index, see
  \link{http://gni.globalnames.org/} for information.
}
\examples{
\dontrun{
gnr(names = c("Helianthus annuus", "Homo sapiens"))
gnr(names = c("Helianthus annuus", "Homo sapiens"), data_source_ids="12")
gnr(names = c("Helianthus annuus", "Homo sapiens"), output="xml")
}
}
\author{
  Scott Chamberlain {myrmecocystus@gmail.com}
}
\seealso{
  \code{\link{tnrastic}} and \code{\link{tnrsmatch}}.
}
\keyword{names}
\keyword{resolve}
\keyword{taxonomy}

