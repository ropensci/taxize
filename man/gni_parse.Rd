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
gni_parse("Plantago minor")
}
}
\references{
\url{http://gni.globalnames.org/}
}
\seealso{
\code{\link{gbif_parse}}
}

