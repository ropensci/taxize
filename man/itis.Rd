\name{itis}
\alias{itis}
\title{Use any API method for the ITIS API.}
\usage{
  itis(query, searchtype = NULL)
}
\arguments{
  \item{query}{Any common or scientific name (character),
  or taxonomic serial number (numeric). Can supply a single
  query or many.}

  \item{searchtype}{Specify one or more of ITIS API methods
  (see \link{http://www.itis.gov/ws_description.html} for
  more information on these methods):
  "getacceptednamesfromtsn","getanymatchcount","getcommentdetailfromtsn",
  "getcommonnamesfromtsn","getcoremetadatafromtsn","getcoveragefromtsn",
  "getcredibilityratingfromtsn","getcurrencyfromtsn","getdatedatafromtsn",
  "getexpertsfromtsn","getfullhierarchyfromtsn","getfullrecordfromtsn",
  "getgeographicdivisionsfromtsn","getglobalspeciescompletenessfromtsn",
  "gethierarchydownfromtsn","gethierarchyupfromtsn","getitistermsfromcommonname",
  "getitistermsfromscientificname","getjurisdictionaloriginfromtsn",
  "getkingdomnamefromtsn","getlsidfromtsn","getothersourcesfromtsn",
  "getparenttsnfromtsn","getpublicationsfromtsn","getreviewyearfromtsn",
  "getscientificnamefromtsn","getsynonymnamesfromtsn","gettaxonauthorshipfromtsn",
  "gettaxonomicranknamefromtsn","gettaxonomicusagefromtsn",
  "getunacceptabilityreasonfromtsn","searchbycommonname",
  "searchbycommonnamebeginswith","searchbycommonnameendswith","searchbyscientificname",
  "searchforanymatch","searchforanymatchpaged".}
}
\value{
  A variety of results can be returned depending on the
  ritis function called.  If many queries or functions, or
  many of both, are provided, results will be returned in a
  list.
}
\description{
  Use any API method for the ITIS API.
}
\examples{
\dontrun{
# Search by scientific name, can abbreviate
itis(query="Plethodon", searchtype="searchbysci")

# Bet an LSID code from TSN code
itis(202420, 'getlsidfromtsn')

# Get the full taxonomic hierarchy for a taxon from the TSN
itis(36616, "getfullhierarchyfromtsn")

# Search by scientific name, then use a TSN to get its parent TSN
itis("Ursus", "searchbyscientificname") # let's pick one of the TSN's
itis(203539, "getparenttsnfromtsn")
itis(203539, "getsynonymnamesfromtsn") # no synonyms in this case

# Use multiple queries on one call to the function
itis(c(203539, 202420), "getsynonymnamesfromtsn")

# Use multiple ritis functions
itis(203539, c("getsynonymnamesfromtsn","getcommonnamesfromtsn"))

# Use multiple ritis functions and multiple queries
itis(c(203539, 202420), searchtype=c("getsynonymnamesfromtsn","getcommonnamesfromtsn"))
}
}

