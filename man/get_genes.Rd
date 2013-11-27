\name{get_genes}
\alias{get_genes}
\title{Retrieve gene sequences from NCBI by accession number.}
\usage{
  get_genes(ids, format = "fasta", verbose = TRUE)
}
\arguments{
  \item{ids}{GenBank ids to search for (character).}

  \item{format}{Return type, e.g., "fasta"}

  \item{verbose}{logical; If TRUE (default), informative
  messages printed.}
}
\value{
  Data.frame of results.
}
\description{
  Retrieve gene sequences from NCBI by accession number.
}
\details{
  Removes predicted sequences so you don't have to remove
  them.  Predicted sequences are those with accession
  numbers that have "XM_" or "XR_" prefixes.
}
\examples{
\dontrun{
# A single gene
get_genes(ids="360040093", format="fasta")

# Many genes (with different accession numbers) for the same species
get_genes(ids=c("360040093","347448433"), format="fasta")
}
}
\author{
  Scott Chamberlain \email{myrmecocystus@gmail.com}
}
\seealso{
  \code{\link[taxize]{get_seqs}},
  \code{\link[taxize]{get_genes_avail}}
}

