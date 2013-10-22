\name{eol_dataobjects}
\alias{eol_dataobjects}
\title{Given the identifier for a data object, return all metadata about the object.}
\usage{
  eol_dataobjects(id, returntype = "list", usekey = TRUE,
    key = NULL)
}
\arguments{
  \item{id}{The EOL data object identifier (character)}

  \item{usekey}{use your API key or not (TRUE or FALSE)}

  \item{returntype}{one of "list" of "data.frame"
  (character)}

  \item{key}{Your EOL API key; loads from .Rprofile.}
}
\value{
  List or dataframe.
}
\description{
  Given the identifier for a data object, return all
  metadata about the object.
}
\details{
  It's possible to return JSON or XML with the EOL API.
  However, this function only returns JSON for now.
}
\examples{
\dontrun{
eol_dataobjects(id="d72801627bf4adf1a38d9c5f10cc767f")
eol_dataobjects(id="21929584")
}
}

