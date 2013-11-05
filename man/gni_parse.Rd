\name{gni_parse}
\alias{gni_parse}
\title{Parse scientific names using EOL's name parser.}
\usage{
  gni_parse(names)
}
\arguments{
  \item{names}{A vector of length 1 or more of taxonomic
  names}
}
\value{
  A data.frame with results, the submitted names, and the
  parsed names with additional information.
}
\description{
  Parse scientific names using EOL's name parser.
}
\examples{
\dontrun{
gni_parse("Cyanistes caeruleus")
gni_parse(names=as.character(df$name))
}
}
\seealso{
  \code{\link{gbif_parse}}
}

