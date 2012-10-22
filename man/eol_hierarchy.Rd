\name{eol_hierarchy}
\alias{eol_hierarchy}
\title{Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.}
\usage{
  eol_hierarchy(taxonConceptID, common_names = NULL,
    synonyms = NULL, usekey = FALSE,
    returntype = "data.frame",
    url = "http://www.eol.org/api/hierarchy_entries/1.0/",
    key = getOption("EOLApi", stop("need an API key for Encyclopedia of Life")))
}
\arguments{
  \item{taxonConceptID}{the EOL page identifier
  (character)}

  \item{common_names}{Return common names or not (defaults
  to returning them, given commonnames=0 if not)}

  \item{synonyms}{Return synonyms or not (defaults to
  returning them, given synonyms=0 if not)}

  \item{usekey}{use your API key or not (TRUE or FALSE)}

  \item{returntype}{one of "list" of "data.frame"
  (character)}

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
  It's possible to return JSON or XML with the EOL API.
  However, this function only returns JSON for now.
}
\examples{
\dontrun{
eol_hierarchy(taxonConceptID=eol_search('Homo')$id[1])
eol_hierarchy(taxonConceptID='39153621', returntype="data.frame")
eol_hierarchy(taxonConceptID='39153621', usekey=T, returntype="data.frame")
}
}

