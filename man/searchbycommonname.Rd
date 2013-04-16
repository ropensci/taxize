\name{searchbycommonname}
\alias{searchbycommonname}
\title{Search for tsn by common name}
\usage{
  searchbycommonname(srchkey = NA, ...,
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
  Search for tsn by common name
}
\examples{
\dontrun{
searchbycommonname("american bullfrog")
searchbycommonname("ferret-badger")
searchbycommonname(srchkey="polar bear")

# Many doing local search
conn <- taxize:::sqlite_init(path="~/github/ropensci/sql/itis2.sqlite")
searchbycommonname(srchkey=c("polar bear", "ferret-badger", "american bullfrog"), locally=TRUE, sqlconn=conn)
searchbycommonname(srchkey=c("common sunflower", "water oak", "american chestnut"), locally=TRUE, sqlconn=conn)
}
}

