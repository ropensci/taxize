\name{eol_hierarchy}
\alias{eol_hierarchy}
\title{Retrieve the taxonomic hierarchy from given EOL taxonID.}
\usage{
  eol_hierarchy(taxonid, common_names = NULL,
    synonyms = NULL, usekey = FALSE,
    returntype = "data.frame", key = NULL)
}
\arguments{
  \item{taxonid}{the EOL page identifier (character)}

  \item{common_names}{Return common names or not (defaults
  to returning them, given commonnames=0 if not)}

  \item{synonyms}{Return synonyms or not (defaults to
  returning them, given synonyms=0 if not)}

  \item{usekey}{use your API key or not (TRUE or FALSE)}

  \item{returntype}{one of "list" of "data.frame"
  (character)}

  \item{key}{Your EOL API key; loads from .Rprofile.}
}
\value{
  List or dataframe of XXXX.
}
\description{
  Retrieve the taxonomic hierarchy from given EOL taxonID.
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
eol_hierarchy(out[out$nameAccordingTo == "Integrated Taxonomic Information
   System (ITIS)", "identifier"])
}
}

