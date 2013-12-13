\name{names_list}
\alias{names_list}
\title{Get a random vector of species names.}
\usage{
names_list(rank = "genus", size = 10)
}
\arguments{
  \item{rank}{Taxonomic rank, one of species, genus
  (default), family, order.}

  \item{size}{Number of names to get. Maximum depends on
  the rank.}
}
\value{
List of taxonomic names
}
\description{
Get a random vector of species names.
}
\examples{
names_list()
names_list('species')
names_list('family')
names_list('order')
names_list('order', '2')
names_list('order', '15')
}
\author{
Scott Chamberlain \email{myrmecocystus@gmail.com}
}

