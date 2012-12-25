\name{fresh_traits}
\alias{fresh_traits}
\title{Query ecological parameters from freshwaterbiology.info}
\usage{
  fresh_traits(x)
}
\arguments{
  \item{x}{tvt-object from fresh_validate}
}
\value{
  data.frame
}
\description{
  Query ecological parameters from freshwaterbiology.info
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
a_traits <- fresh_traits(a)
a_traits

### select only respiration-modalities using lookup table
data(fresh_desc)
take = fresh_desc$Modality[fresh_desc$Trait == "respiration"]
qu[ ,take]
}
}
\author{
  Eduard Szoecs \email{szoe8822@uni-landau.de}
}

