\name{eol_search}
\alias{eol_search}
\title{Search for terms in EOL database.}
\usage{
  eol_search(terms, usekey = FALSE,
    returntype = "data.frame",
    url = "http://eol.org/api/search/",
    key = getOption("EOLApi", stop("need an API key for Encyclopedia of Life")))
}
\arguments{
  \item{terms}{search terms (character)}

  \item{usekey}{use your API key or not (TRUE or FALSE)}

  \item{returntype}{one of "list" of "data.frame"
  (character)}

  \item{url}{The EOL url for the function (should be left
  to default).}

  \item{key}{Your EOL API key; loads from .Rprofile.}
}
\value{
  JSON list object, or data.frame.
}
\description{
  Search for terms in EOL database.
}
\details{
  It's possible to return JSON or XML with the EOL API.
  However, this function only returns JSON for now.
}
\examples{
\dontrun{
eol_search('Homo')
eol_search('Salix')
eol_search(terms='Ursus americanus luteolus')
}
}

