\name{gnr_datasources}
\alias{gnr_datasources}
\title{Get data sources for the Global Names Resolver.}
\usage{
  gnr_datasources(todf = FALSE,
    url = "http://resolver.globalnames.org/data_sources.json")
}
\arguments{
  \item{todf}{Parse id and name of provider to data.frame?
  logical (default = FALSE).}

  \item{url}{Base url for the API; leave as is.}
}
\value{
  json or xml output, your choice
}
\description{
  Uses the Global Names Index, see
  \url{http://gni.globalnames.org/} for information.
}
\examples{
\dontrun{
# all data sources
gnr_datasources()

# just id's and names of sources
gnr_datasources(todf=T)

# give me the id for EOL
out <- gnr_datasources(todf=T)
out[out$title == "EOL", "id"]

# Fuzzy search for sources with the word zoo
out <- gnr_datasources(todf=T)
out[agrep("zoo", out$title, ignore.case=T), ]
}
}
\author{
  Scott Chamberlain {myrmecocystus@gmail.com}
}
\seealso{
  \code{\link{iplant_tnrsmatch}}.
}
\keyword{names}
\keyword{resolve}
\keyword{taxonomy}

