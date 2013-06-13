\name{iucn_summary}
\alias{iucn_summary}
\title{Get a summary from the IUCN Red List.}
\usage{
  iucn_summary(sciname, silent = TRUE, parallel = FALSE)
}
\arguments{
  \item{sciname}{character; Scientific name.}

  \item{silent}{Make errors silent or not (when species not
  found in IUCN), default TRUE.}

  \item{parallel}{Search in parallel to speed up search.
  Defaults to FALSE.  You have to register a parallel
  backend if you set parallel=TRUE. See e.g., doMC, doSNOW,
  etc.}
}
\value{
  A list (for every species one entry) of lists with the
  following items: \item{status}{Red List Category.}
  \item{history}{History of status.}
  \item{distr}{Geographic distribution.} \item{trend}{Trend
  of population size.}
}
\description{
  Get a summary from the IUCN Red List
  (\url{http://www.iucnredlist.org/}).
}
\examples{
\dontrun{
ia <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
ia <- iucn_summary(c("Panthera uncia", "Lynx lynx", "aaa"))
# extract status
ldply(ia, function(x) x$status)
laply(ia, function(x) x$status)
ia[['Lynx lynx']]$history
ia[['Panthera uncia']]$distr
ia[[1]]$pop
ia[[2]]$trend
}
}
\author{
  Eduard Szoecs \email{szoe8822@uni-landau.de}
}

