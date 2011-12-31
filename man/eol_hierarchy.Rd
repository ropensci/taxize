\name{eol_hierarchy}
\alias{eol_hierarchy}
\title{Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.}
\usage{
  eol_hierarchy(taxonConceptID, json = FALSE,
    usekey = FALSE, returntype = "list",
    url = "http://www.eol.org/api/hierarchy_entries/1.0/",
    key = getOption("EOLApi", stop("need an API key for Encyclopedia of Life")))
}
\arguments{
  \item{taxonConceptID}{the EOL page identifier
  (character)}

  \item{json}{return in json format (TRUE or FALSE), if
  FALSE, returns XML}

  \item{usekey}{use your API key or not (TRUE or FALSE)}

  \item{returntype}{one of list of dataframe (character)}

  \item{url}{The EOL url for the function (should be left
  to default).}

  \item{key}{Your EOL API key; loads from .Rprofile.}
}
\value{
  List or dataframe of XXXX.
}
\description{
  Retrieve all taxa names or TSNs downstream in hierarchy
  from given TSN.
}
\details{
  Can't seem to get json format results along with
  specifiying an API key, so if you use json your key is
  not specified at the moment
}
\examples{
\dontrun{
eol_hierarchy('34345893')
eol_hierarchy('34345893', json = 'TRUE')
}
}

