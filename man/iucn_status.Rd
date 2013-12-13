\name{iucn_status}
\alias{iucn_status}
\title{Extractor functions for \code{iucn}-class.}
\usage{
iucn_status(x, ...)
}
\arguments{
  \item{x}{an \code{iucn}-object as returned by
  \code{iucn_summary}}

  \item{...}{Currently not used}
}
\value{
A character vector with the status.
}
\description{
Extractor functions for \code{iucn}-class.
}
\examples{
\dontrun{
ia <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
iucn_status(ia)}
}
\seealso{
\code{\link[taxize]{iucn_summary}}
}

