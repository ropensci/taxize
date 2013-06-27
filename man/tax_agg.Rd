\name{tax_agg}
\alias{print.tax_agg}
\alias{tax_agg}
\title{Aggregate species data by given taxonomic rank}
\usage{
  tax_agg(x, rank, db = "ncbi")

  \method{print}{tax_agg} (x, ...)
}
\arguments{
  \item{x}{Community data matrix.}

  \item{character;}{rank Taxonomic rank to aggregate by.}

  \item{character;}{taxonomic API to use, 'ncbi or 'itis'.}
}
\value{
  A list of class \code{tax_agg} with the following items:
  \item{x}{Community data matrix with aggregated data.}
  \item{by}{A lookup-table showing which taxa where
  aggregated} \item{n_pre}{Number of taxa before
  aggregation} \item{rank}{Rank at which taxa have been
  aggregated}
}
\description{
  Aggregate species data by given taxonomic rank
}
\examples{
# use dune dataset
data(dune, package='vegan')
species <- c("Bellis perennis", "Empetrum nigrum", "Juncus bufonius", "Juncus articulatus",
"Aira praecox", "Eleocharis parvula", "Rumex acetosa", "Vicia lathyroides",
"Brachythecium rutabulum", "Ranunculus flammula", "Cirsium arvense",
"Hypochaeris radicata", "Leontodon autumnalis", "Potentilla palustris",
"Poa pratensis", "Calliergonella cuspidata", "Trifolium pratense",
"Trifolium repens", "Anthoxanthum odoratum", "Salix repens", "Achillea millefolium",
"Poa trivialis", "Chenopodium album", "Elymus repens", "Sagina procumbens",
"Plantago lanceolata", "Agrostis stolonifera", "Lolium perenne", "Alopecurus geniculatus",
"Bromus hordeaceus")
colnames(dune) <- species

# aggregate sample to families
agg <- tax_agg(dune, rank = 'family', db = 'ncbi')
agg

# extract aggregated community data matrix
agg$x
# check whcih taxa have been aggregated
agg$by
}

