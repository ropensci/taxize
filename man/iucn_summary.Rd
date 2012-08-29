\name{iucn_summary}
\alias{iucn_summary}
\title{Get a summary from the IUCN Redlist.}
\usage{
  iucn_summary(sciname)
}
\arguments{
  \item{sciname}{Scientific name}
}
\value{
  A list with the following items: \item{status}{Red List
  Category} \item{history}{History of status}
  \item{distr}{Geographic distribution}
  \item{pop}{Population size estimates} \item{trend}{Trend
  of population size}
}
\description{
  Get a summary from the IUCN Redlist.
}
\examples{
\dontrun{
ia <- iucn_summary("Panthera uncia")
ia$status
ia$history
ia$distr
ia$pop
ia$trend
}
}

