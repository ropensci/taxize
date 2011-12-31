\name{eol_search}
\alias{eol_search}
\title{Search for terms in EOL database.}
\usage{
  eol_search(terms, json = FALSE, usekey = FALSE,
    url = "http://www.eol.org/api/search/",
    key = getOption("EOLApi", stop("need an API key for Encyclopedia of Life")))
}
\arguments{
  \item{terms}{search terms (character)}

  \item{json}{return in json format (TRUE or FALSE), if
  FALSE, returns XML}

  \item{usekey}{use your API key or not (TRUE or FALSE)}

  \item{url}{The EOL url for the function (should be left
  to default).}

  \item{key}{Your EOL API key; loads from .Rprofile.}
}
\value{
  XML or JSON object.
}
\description{
  Search for terms in EOL database.
}
\details{
  Can't seem to get json format results along with
  specifiying an API key, so if you use json your key is
  not specified at the moment.  Also, the 'page' option is
  not currently implemented as it doesn't seem useful, so
  if you use json your key is not specified at the moment
}
\examples{
\dontrun{
eol_search('Homo')
eol_search('Salix')
}
}

