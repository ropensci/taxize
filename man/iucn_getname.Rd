\name{iucn_getname}
\alias{iucn_getname}
\title{Get any matching IUCN species names}
\usage{
  iucn_getname(name, verbose = TRUE, ...)
}
\arguments{
  \item{name}{}

  \item{verbose}{defaults to TRUE}

  \item{...}{Further arguments passed on to
  \code{link{iucn_summary}}}
}
\description{
  Get any matching IUCN species names
}
\examples{
\dontrun{
iucn_getname(name = "Cyanistes caeruleus")
}
}
\seealso{
  \code{\link{iucn_summary}} \code{\link{iucn_status}}
}

