\name{gettsnbyvernacularlanguage}
\alias{gettsnbyvernacularlanguage}
\title{Retrieve accepted TSN (with accepted name)}
\usage{
  gettsnbyvernacularlanguage(language = NA, ...,
    curl = getCurlHandle(), locally = FALSE)
}
\arguments{
  \item{language}{A string containing the language. This is
  a language string, not the international language code
  (character)}

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
  Retrieve accepted TSN (with accepted name)
}
\examples{
\dontrun{
# Using web API
gettsnbyvernacularlanguage("french")

# Using local sql - one
gettsnbyvernacularlanguage("french", locally=TRUE)

# Using local sql - many
gettsnbyvernacularlanguage(language=c("french","german"), locally=TRUE)
}
}

