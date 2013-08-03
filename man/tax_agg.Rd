\name{tax_agg}
\alias{print.tax_agg}
\alias{tax_agg}
\title{Aggregate species data to given taxonomic rank}
\usage{
  tax_agg(x, rank, db = "ncbi", ...)

  \method{print}{tax_agg} (x, ...)
}
\arguments{
  \item{x}{Community data matrix. Taxa in columns, samples
  in rows.}

  \item{rank}{character; Taxonomic rank to aggregate by.}

  \item{db}{character; taxonomic API to use, 'ncbi, 'itis'
  or both, see \code{\link[taxize]{tax_name}}.}

  \item{...}{other arguments passed to
  \code{\link[taxize]{tax_name}}.}
}
\value{
  A list of class \code{tax_agg} with the following items:
  \item{x}{Community data matrix with aggregated data.}
  \item{by}{A lookup-table showing which taxa were
  aggregated.} \item{n_pre}{Number of taxa before
  aggregation.} \item{rank}{Rank at which taxa have been
  aggregated.}
}
\description{
  Aggregate species data to given taxonomic rank
}
\details{
  \code{tax_agg} aggregates (sum) taxa to a specific
  taxonomic level. If a taxon is not found in the database
  (ITIS or NCBI) or the supplied taxon is on higher
  taxonomic level this taxon is not aggregated.
}
\examples{
\dontrun{
# use dune dataset
data(dune, package='vegan')
species <- c("Bellis perennis", "Empetrum nigrum", "Juncus bufonius",
"Juncus articulatus",
"Aira praecox", "Eleocharis parvula", "Rumex acetosa", "Vicia lathyroides",
"Brachythecium rutabulum", "Ranunculus flammula", "Cirsium arvense",
"Hypochaeris radicata", "Leontodon autumnalis", "Potentilla palustris",
"Poa pratensis", "Calliergonella cuspidata", "Trifolium pratense",
"Trifolium repens", "Anthoxanthum odoratum", "Salix repens", "Achillea
millefolium",
"Poa trivialis", "Chenopodium album", "Elymus repens", "Sagina procumbens",
"Plantago lanceolata", "Agrostis stolonifera", "Lolium perenne", "Alopecurus
geniculatus",
"Bromus hordeaceus")
colnames(dune) <- species

# aggregate sample to families
agg <- tax_agg(dune, rank = 'family', db = 'ncbi')
agg

# extract aggregated community data matrix for further usage
agg$x
# check which taxa have been aggregated
agg$by
}
}
\seealso{
  \code{\link[taxize]{tax_name}}
}

