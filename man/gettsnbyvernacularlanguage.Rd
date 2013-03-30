\name{gettsnbyvernacularlanguage}
\alias{gettsnbyvernacularlanguage}
\title{Retrieve accepted TSn(with accepted name)}
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
}
\description{
  Retrieve accepted TSn(with accepted name)
}
\examples{
\dontrun{
gettsnbyvernacularlanguage("french")
}
}

