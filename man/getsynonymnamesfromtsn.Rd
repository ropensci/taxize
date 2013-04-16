\name{getsynonymnamesfromtsn}
\alias{getsynonymnamesfromtsn}
\title{Returns a list of the synonyms (if any) for the TSN.}
\usage{
  getsynonymnamesfromtsn(tsn = NA, ...,
    curl = getCurlHandle(), locally = FALSE,
    returnindex = FALSE)
}
\arguments{
  \item{returnindex}{Retrun the index of each searched
  string with the resulting data.frame.  Useful in get_tsn
  to split results by searched string. Useful if you need
  to know what results (i.e., rows) are associated with
  which queries.}

  \item{tsn}{TSN for a taxonomic group (numeric)}

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
  Returns a list of the synonyms (if any) for the TSN.
}
\examples{
\dontrun{
# Web API
getsynonymnamesfromtsn(tsn = 183671) # tsn not accepted
getsynonymnamesfromtsn(tsn = 526852) # tsn accepted

# many with web API
ldply(c(202385,36616,19370,41107), getsynonymnamesfromtsn)

# Local search with sql
taxize:::taxize_options(localpath = "~/github/ropensci/sql/itis2.sqlite")
getsynonymnamesfromtsn(tsn = 526852, locally=TRUE)
getsynonymnamesfromtsn(tsn=c(202385,36616,19370,41107), locally=TRUE)
}
}

