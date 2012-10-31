\name{get_genes_avail}
\alias{get_genes_avail}
\title{Retrieve gene sequences available for a species from NCBI.}
\usage{
  get_genes_avail(taxon_name, seqrange, getrelated = FALSE)
}
\arguments{
  \item{taxon_name}{Scientific name to search for
  (character).}

  \item{seqrange}{Sequence range, as e.g., "1:1000"
  (character).}

  \item{getrelated}{Logical, if TRUE, gets the longest
  sequences of a species in the same genus as the one
  searched for. If FALSE, get's nothing.}
}
\value{
  Data.frame of results.
}
\description{
  This function retrieves one sequences for each species,
  picking the longest available for the given gene.
}
\details{
  Removes predicted sequences so you don't have to remove
  them.  Predicted sequences are those with accession
  numbers that have "XM_" or "XR_" prefixes.
}
\examples{
\dontrun{
# A single species
out <- get_genes_avail(taxon_name="Umbra limi", seqrange = "1:2000", getrelated=F)
unique(out$genesavail) # get list of genes available, removing non-unique ones
out[grep("RAG1", out$genesavail, ignore.case=T),] # does the string 'RAG1' exist in any of the gene names

# Many species, can run in parallel or not using plyr
species <- c("Salvelinus alpinus","Ictalurus nebulosus","Carassius auratus")
out2 <- llply(species, get_genes_avail, seqrange = "1:2000", getrelated=F)
lapply(out2, head) # see heads of all
out2df <- ldply(out2) # make data.frame of all
unique(out2df$genesavail) # get list of genes available, removing non-unique ones
out2df[grep("RAG1", out2df$genesavail, ignore.case=T),] # search across all
}
}
\author{
  Scott Chamberlain \email{myrmecocystus@gmail.com}
}

