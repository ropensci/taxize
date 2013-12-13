\name{ubio_id}
\alias{ubio_id}
\title{Search uBio by namebank ID.}
\usage{
ubio_id(namebankID = NULL, keyCode = NULL, callopts = list())
}
\arguments{
  \item{namebankID}{(string) - uBio namebank ID}

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
Search uBio by namebank ID.
}
\examples{
\dontrun{
ubio_id(namebankID = 2483153)
}
}

