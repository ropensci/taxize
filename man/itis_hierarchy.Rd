\name{itis_hierarchy}
\alias{itis_hierarchy}
\title{Get hierarchies from TSN values, full, upstream only, or immediate downstream
only}
\usage{
  itis_hierarchy(tsn = NULL, what = "full", ...)
}
\arguments{
  \item{tsn}{One or more TSN's (taxonomic serial number)}

  \item{what}{One of full (full hierarchy), up (immediate
  upstream), or down (immediate downstream)}

  \item{...}{Further arguments passed on to
  \code{\link{getjurisdictionaloriginfromtsn}}}
}
\description{
  Get hierarchies from TSN values, full, upstream only, or
  immediate downstream only
}
\details{
  Note that \code{\link{itis_downstream}} gets taxa
  downstream to a particular rank, whilc this function only
  gets immediate names downstream.
}
\examples{
\dontrun{
# Get full hierarchy
itis_hierarchy(tsn=180543)

# Get hierarchy upstream
itis_hierarchy(tsn=180543, "up")

# Get hierarchy downstream
itis_hierarchy(tsn=180543, "down")

# Many tsn's
itis_hierarchy(tsn=c(180543,41074,36616))

# Suppress messages
itis_hierarchy(c(180543,41074,36616), verbose=FALSE)
}
}
\seealso{
  \code{\link{itis_downstream}}
}

