\name{ubio_namebank}
\alias{ubio_namebank}
\title{Search uBio namebank.}
\usage{
  ubio_namebank(searchName = NULL, searchAuth = NULL,
    searchYear = NULL, order = NULL, sci = NULL,
    vern = NULL, keyCode = NULL)
}
\arguments{
  \item{searchName}{(string) - term to search within name
  string}

  \item{searchAuth}{(string) - term to search within name
  authorship}

  \item{searchYear}{(string) - term to search within name
  year}

  \item{order}{(string) - (name or namebankID) field by
  which the results will be sorted (default is namebankID)}

  \item{sci}{(int) - (sci, vern, or all) type of results to
  be returned (default is all)}

  \item{vern}{(int) - (limit 1000) maximum number of
  results to be returned (default is 1000)}

  \item{keyCode}{Your uBio API key; loads from .Rprofile.
  If you don't have one, obtain one at
  http://www.ubio.org/index.php?pagename=form.}
}
\value{
  List or dataframe of XXXX.
}
\description{
  Search uBio namebank.
}
\details{
  Can't seem to get json format results along with
  specifiying an API key, so if you use json your key is
  not specified at the moment
}
\examples{
\dontrun{
ubio_namebank(searchName = 'elephant', sci = 1, vern = 0)
ubio_namebank(searchName = 'Astragalus aduncus', sci = 1, vern = 0)
}
}

