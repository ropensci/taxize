\name{plantminer}
\alias{plantminer}
\title{Search for taxonomy data from Plantminer.com}
\usage{
  plantminer(plants, key = NULL)
}
\arguments{
  \item{plants}{Vector of plant species names.}

  \item{key}{Your api key for the plantminer.com site.  Go
  to \url{http://www.plantminer.com/} to get your api key.
  Two options for inputting your key.  1) You can input it
  manually within the function as the second argument, or
  2) you can put the key in your .Rprofile file, which will
  then be loaded when you start R. See
  \url{https://github.com/ropensci/rOpenSci/wiki/Installation-and-use-of-API-keys}
  for help on how to put api keys in your .Rprofile file.}
}
\value{
  data.frame of results.
}
\description{
  Search for taxonomy data from Plantminer.com
}
\examples{
\dontrun{
plants <- c("Myrcia lingua", "Myrcia bella", "Ocotea pulchella",
		"Miconia", "Coffea arabica var. amarella", "Bleh")
plantminer(plants)
}
}

