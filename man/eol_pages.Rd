\name{eol_pages}
\alias{eol_pages}
\title{Search for pages in EOL database using a taxonconceptID.}
\usage{
  eol_pages(taxonconceptID, usekey = FALSE,
    returntype = "data.frame",
    url = "http://eol.org/api/pages/1.0/", key = NULL)
}
\arguments{
  \item{taxonconceptID}{The taxonconceptID (numeric), which
  is also the page number.}

  \item{usekey}{Use your API key or not (TRUE or FALSE)}

  \item{returntype}{One of "list" of "data.frame"
  (character)}

  \item{url}{The EOL url for the function (should be left
  to default).}

  \item{key}{Your EOL API key; loads from .Rprofile, or you
  can specify the key manually the in the function call.}
}
\value{
  JSON list object, or data.frame.
}
\description{
  Search for pages in EOL database using a taxonconceptID.
}
\details{
  It's possible to return JSON or XML with the EOL API.
  However, this function only returns JSON for now.
}
\examples{
\dontrun{
pageid <- eol_search('Pomatomus')$id[1]
out <- eol_pages(taxonconceptID=pageid)
eol_hierarchy(out[out$nameAccordingTo == "NCBI Taxonomy", "identifier"])
eol_hierarchy(out[out$nameAccordingTo == "Integrated Taxonomic Information System (ITIS)", "identifier"])
}
}

