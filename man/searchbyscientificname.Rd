\name{searchbyscientificname}
\alias{searchbyscientificname}
\title{Search by scientific name}
\usage{
  searchbyscientificname(srchkey = NA, ...,
    curl = getCurlHandle(), locally = FALSE,
    sqlconn = NULL, returnindex = FALSE)
}
\arguments{
  \item{returnindex}{Retrun the index of each searched
  string with the resulting data.frame.  Useful in get_tsn
  to split results by searched string.}

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

  \item{sqlconn}{The sqlite3 connection object.}
}
\description{
  Search by scientific name
}
\examples{
\dontrun{
searchbyscientificname("Tardigrada")

# Using the web API, have to submit one at a time
ldply(c("oryza sativa","Chironomus riparius","Helianthus annuus","Quercus lobata"), searchbyscientificname)

# Using local search, can submit many in one vector
searchbyscientificname(srchkey=c("oryza sativa","Chironomus riparius","Helianthus annuus","Quercus lobata"), locally=TRUE, sqlconn=conn)

searchbyscientificname("Quercus douglasii", locally=TRUE, sqlconn=conn)
}
}

