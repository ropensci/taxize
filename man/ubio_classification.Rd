\name{ubio_classification}
\alias{ubio_classification}
\title{This function will return all ClassificationBank data pertaining to a
particular ClassificationBankID}
\usage{
  ubio_classification(hierarchiesID = NULL,
    childrenFlag = NULL, ancestryFlag = NULL,
    justificationFlag = NULL, synonymsFlag = NULL,
    keyCode = NULL, callopts = list())
}
\arguments{
  \item{hierarchiesID}{(string) - ClassificationBank
  identifier for the record you wish to receive}

  \item{childrenFlag}{(0 or 1) to include the taxon's
  children taxa}

  \item{ancestryFlag}{(0 or 1) to include the taxon's
  taxonomic ancestry}

  \item{justificationFlag}{(0 or 1) to include the
  bibliographic references}

  \item{synonymsFlag}{(0 or 1) to include the taxon's
  synonymous taxa}

  \item{keyCode}{Your uBio API key; loads from .Rprofile.
  If you don't have one, obtain one at
  http://www.ubio.org/index.php?pagename=form.}

  \item{callopts}{Parameters passed on to httr::GET call.}
}
\value{
  A list of four data.frame's, one for the name itself, one
  for synonyms, one for vernacular names, and one for
  citations.
}
\description{
  This function will return all ClassificationBank data
  pertaining to a particular ClassificationBankID
}
\examples{
\dontrun{
ubio_classification(hierarchiesID = 2483153)
}
}

