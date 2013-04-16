\name{searchbycommonnameendswith}
\alias{searchbycommonnameendswith}
\title{Search for tsn by common name ending with}
\usage{
  searchbycommonnameendswith(srchkey = NA, ...,
    curl = getCurlHandle(), locally = FALSE,
    returnindex = FALSE)
}
\arguments{
  \item{returnindex}{Retrun the index of each searched
  string with the resulting data.frame.  Useful in get_tsn
  to split results by searched string. Useful if you need
  to know what results (i.e., rows) are associated with
  which queries.}

  \item{srchkey}{text or taxonomic serial number (TSN)
  (character or numeric)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}

  \item{locally}{If TRUE, queries are run locally in
  sqlite3; if FALSE (the default), queries are run against
  the ITIS web API. locally=TRUE should be faster in almost
  all cases.}
}
\description{
  Search for tsn by common name ending with
}
\examples{
\dontrun{
searchbycommonnameendswith(srchkey="snake")

# Many names, locally
conn <- taxize:::sqlite_init(path="~/github/ropensci/sql/itis2.sqlite")
searchbycommonnameendswith(srchkey=c("bear","snake","duck"), locally=TRUE, sqlconn=conn)
}
}

