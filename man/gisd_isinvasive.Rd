\name{gisd_isinvasive}
\alias{gisd_isinvasive}
\title{Check invasive species status for a set of species from GISD database}
\usage{
  is.invasive(sp, simplified.df = FALSE)
}
\arguments{
  \item{sp}{a vector of species names in latin of the form c("Genus species")}

  \item{simplified.df}{Returns a data.frame with the species name and the values "Invasive", "Not in GISD". I recomend to check first the not simplified version (default), which contains raw information about the level of invasiveness.}
}
\value{
  Dataframe
}
\description{This function check which species (both plants and animals) are considered "invaders" somewhere in the world.  For that end, it checks GISD (http://www.issg.org/database/welcome/) and returns a value, either "Not in GISD" or the brief description presented in GISD. Note that the webpage contains more information. Also note that the function won't tell you if it's exotic in your area, a lot of exotic species are not considered invaders (yet). As expected, the function is as good as the database is, which I find quite reliable and well maintained. The database is also able to recognize a lot (but not all) of the species synonyms.
}
\details{This function worked for me, but I didn't test it intensively, and any changes on the webpage html design will return wrong values. Apply the usual disclaimers when using it. The function may be slow (not optimized), so be patient with long lists of species.
}
\author{Ignasi Bartomeus (nacho.bartomeus#gmail.com)}

\examples{
sp <- c("Carpobrotus edulis", "Rosmarinus officinalis")
## first species is invasive, second one is not. 
is.invasive(sp)
is.invasive(sp, simplified.df = TRUE)
}

