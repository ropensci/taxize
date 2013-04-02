\name{searchforanymatchpaged}
\alias{searchforanymatchpaged}
\title{Search for any matched page}
\usage{
  searchforanymatchpaged(srchkey = NA, pagesize = NA,
    pagenum = NA, ascend = NA, ..., curl = getCurlHandle(),
    locally = FALSE)
}
\arguments{
  \item{pagesize}{An integer containing the page size
  (numeric)}

  \item{pagenum}{An integer containing the page number
  (numeric)}

  \item{ascend}{A boolean containing true for ascending
  sort order or false for descending (logical)}

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
  Search for any matched page
}
\examples{
\dontrun{
searchforanymatchpaged(202385, 100, 1, FALSE)
}
}

