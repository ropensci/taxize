\name{searchforanymatchpaged}
\alias{searchforanymatchpaged}
\title{Search for any matched page}
\usage{
searchforanymatchpaged(srchkey = NA, pagesize = NA, pagenum = NA,
  ascend = NA, ..., curl = getCurlHandle())
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
}
\description{
Search for any matched page
}
\examples{
\dontrun{
searchforanymatchpaged(srchkey=202385, pagesize=100, pagenum=1, ascend=FALSE)
searchforanymatchpaged(srchkey="Zy", pagesize=100, pagenum=1, ascend=FALSE)
}
}
\keyword{internal}

