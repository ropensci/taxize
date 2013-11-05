\name{ubio_synonyms}
\alias{ubio_synonyms}
\title{Search uBio by namebank ID.}
\usage{
  ubio_synonyms(hierarchiesID = NULL, keyCode = NULL,
    callopts = list())
}
\arguments{
  \item{hierarchiesID}{you must include the hierarchiesID
  (ClassificationBankID) to receive the classification
  synonyms}

  \item{keyCode}{Your uBio API key; loads from .Rprofile.
  If you don't have one, obtain one at
  http://www.ubio.org/index.php?pagename=form.}

  \item{callopts}{Parameters passed on to httr::GET call.}
}
\value{
  A data.frame.
}
\description{
  Search uBio by namebank ID.
}
\examples{
\dontrun{
ubio_synonyms(hierarchiesID = 4091702)
}
}

