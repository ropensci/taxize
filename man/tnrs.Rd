\name{tnrs}
\alias{tnrs}
\title{Search the Phylotastic Taxonomic Name Resolution Service.}
\usage{
tnrs(query = NA, source = NULL, code = NULL, getpost = "POST",
  sleep = 0, splitby = 30, verbose = TRUE)
}
\arguments{
  \item{query}{Quoted taxonomic names to search in a vector
  (character).}

  \item{source}{Specify the source you want to match names
  against. Defaults to just retrieve data from all sources.
  Options: NCBI, iPlant_TNRS, or MSW3. Only available when
  using getpost="POST".}

  \item{code}{Nomenclatural code. One of: ICZN
  (zoological), ICN (algae, fungi, and plants), ICNB
  (bacteria), ICBN (botanical), ICNCP (cultivated plants),
  ICTV (viruses). Only available when using
  getpost="POST".}

  \item{getpost}{Use GET or POST method to send the query.
  If you have more than say 50 species or so in your query,
  you should probably use POST. POST is the only option for
  this parameter if you want to use source or code
  parameters.}

  \item{sleep}{Numer of seconds by which to pause between
  calls. Defaults to 0 seconds. Use when doing many calls
  in a for loop ar lapply type call.}

  \item{splitby}{Number by which to split species list for
  querying the TNRS.}

  \item{verbose}{Verbosity or not (default TRUE)}
}
\value{
data.frame of results from TNRS plus the name submitted.
}
\description{
Match taxonomic names using the Taxonomic Name Resolution
Service (TNRS).  Returns score of the matched name, and
whether it was accepted or not.
}
\details{
If there is no match in the Taxosaurus database, nothing is
returned, so youwill not get anything back for non matches.
}
\examples{
\dontrun{
# Default, uses GET curl method, you can't specify any other parameters when
using GET
mynames <- c("Panthera tigris", "Neotamias minimus", "Magnifera indica")
tnrs(query = mynames)

# Specifying the source to match against
mynames <- c("Helianthus annuus", "Poa annua")
tnrs(query = mynames, source = "iPlant_TNRS")

# Specifying the nomenclatural code to match against
mynames <- c("Helianthus annuus", "Poa annua")
tnrs(query = mynames, code = "ICBN")

# You can specify multiple sources, by comma-separating them
mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica",
"Humbert humbert")
tnrs(query = mynames, source = "NCBI,MSW3")

# Using POST method, especially useful when you have a lot of species
mynames <- c("Panthera tigris", "Eutamias minimus", "Magnifera indica",
   "Humbert humbert", "Helianthus annuus", "Pinus contorta", "Poa annua",
   "Abies magnifica", "Rosa california", "Festuca arundinace",
   "Mimulus bicolor", "Sorbus occidentalis","Madia sativa", "Thymopsis thymodes",
   "Bartlettia scaposa")
tnrs(mynames, source = "NCBI")

# And even more names
mynames <- names_list(rank="species", size=75)
tnrs(query=mynames, source = "NCBI")
## Or use splitby
tnrs(mynames, source = "NCBI", splitby=50)
}
}

