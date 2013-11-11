\name{searchCommon}
\alias{searchCommon}
\title{searchCommon}
\usage{
  searchCommon(srchkey = NA, ..., curl = getCurlHandle(),
    from = "begin")
}
\arguments{
  \item{srchkey}{search terms}

  \item{...}{additional arguments}

  \item{curl}{curl handle}

  \item{from}{default is to search from beginning. Use
  \code{end} to serch from end.}
}
\value{
  \code{data.frame}
}
\description{
  Searches common name and acts as thin wrapper around
  \code{searchbycommonnamebeginswith} and
  \code{searchbycommonnameendswith}
}
\examples{
\dontrun{
searchCommon(srchkey="inch")
searchCommon(srchkey="inch", from = "end")
}
}
\seealso{
  searchbycommonnamebeginswith searchbycommonnameendswith
}

