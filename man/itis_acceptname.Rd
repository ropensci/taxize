\name{itis_acceptname}
\alias{itis_acceptname}
\title{Retrieve accepted TSN (with accepted name).}
\usage{
itis_acceptname(searchtsn = NA, ...)
}
\arguments{
  \item{searchtsn}{Quoted TSN for a taxonomic group
  (character).}

  \item{...}{Further arguments passed on to
  getacceptednamesfromtsn}
}
\value{
Names or TSNs of all downstream taxa.
}
\description{
Retrieve accepted TSN (with accepted name).
}
\details{
You can print informative messages by setting
supmess=FALSE.
}
\examples{
\dontrun{
itis_acceptname('208527')  # TSN accepted - good name
itis_acceptname('504239')  # TSN not accepted - input TSN is old
}
}

