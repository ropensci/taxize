\name{fresh_codes}
\alias{fresh_codes}
\title{Query Codes from freshwaterbiology.info}
\usage{
  fresh_codes(x)
}
\arguments{
  \item{x}{tvt-object from fresh_validate}
}
\value{
  data.frame
}
\description{
  Query Codes from freshwaterbiology.info
}
\note{
  Currently only the Macro-invertebrate database is
  supported.
}
\examples{
\dontrun{
spec <- c("Acentrella sinaica",
"Acentria ephemerella",
"Acilius sp.",
"Acroloxus lacustris",
"Allotrichi pallicornis")

a <- fresh_validate(spec)
fresh_codes(a)
}
}
\author{
  Eduard Szoecs \email{szoe8822@uni-landau.de}
}

