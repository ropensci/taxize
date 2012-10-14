\name{iucn_summary}
\alias{iucn_summary}
\title{Get a summary from the IUCN Red List.}
\usage{
  iucn_summary(sciname)
}
\arguments{
  \item{sciname}{Scientific name.}
}
\value{
  A list (for every species one entry) of lists with the
  following items: \item{status}{Red List Category.}
  \item{history}{History of status.}
  \item{distr}{Geographic distribution.}
  \item{pop}{Population size estimates.} \item{trend}{Trend
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
  Eduard Sz??cs \email{szoe8822@uni-landau.de}
}

