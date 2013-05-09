\name{gnr_resolve}
\alias{gnr_resolve}
\title{Resolve names using Global Names Resolver.}
\usage{
  gnr_resolve(names, data_source_ids = NULL,
    resolve_once = FALSE, with_context = FALSE,
    stripauthority = FALSE, highestscore = TRUE)
}
\arguments{
  \item{names}{Quoted taxonomic names to be resolved in a
  vector.}

  \item{data_source_ids}{Supply data source IDs to specify
  what data source is searched. See example below.}

  \item{resolve_once}{Default is false. Find the first
  available match instead of matches across all data
  sources with all possible renderings of a name.  When
  'true', response is rapid but incomplete (logical)}

  \item{with_context}{Reduce the likelihood of matches to
  taxonomic homonyms.  When 'true' a common taxonomic
  context is calculated for all supplied names from matches
  in data sources that have classification tree paths.
  Names out of determined context are penalized during
  score calculation.}

  \item{stripauthority}{If TRUE, gives back names with
  taxonomic authorities. If FALSE, strips author names.}

  \item{highestscore}{Return those names with the highest
  score for each searched name.}
}
\value{
  A data.frame.
}
\description{
  Uses the Global Names Index, see
  \url{http://gni.globalnames.org/} for information.
}
\examples{
\dontrun{
gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"), returndf = TRUE)
gnr_resolve(names = c("Asteraceae", "Plantae"), returndf = TRUE)

# Using data source 12 (Encyclopedia of Life)
gnr_resolve(names = c("Helianthos annuus", "Homo sapians"), returndf = TRUE)
}
}
\author{
  Scott Chamberlain {myrmecocystus@gmail.com}
}
\keyword{names}
\keyword{resolve}
\keyword{taxonomy}

