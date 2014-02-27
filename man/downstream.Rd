\name{downstream}
\alias{downstream}
\alias{downstream.colid}
\alias{downstream.default}
\alias{downstream.ids}
\alias{downstream.tsn}
\title{Retrieve the downstream taxa for a given taxon name or ID.}
\usage{
downstream(...)

\method{downstream}{default}(x, db = NULL, downto = NULL, ...)

\method{downstream}{tsn}(x, db = NULL, ...)

\method{downstream}{colid}(x, db = NULL, ...)

\method{downstream}{ids}(x, db = NULL, downto, ...)
}
\arguments{
  \item{x}{character; taxons to query.}

  \item{db}{character; database to query. One or both of
  \code{itis}, \code{col}.}

  \item{downto}{What taxonomic rank to go down to. One of:
  'Superkingdom','Kingdom',
  'Subkingdom','Infrakingdom','Phylum','Division','Subphylum','Subdivision','Infradivision',
  'Superclass','Class','Subclass','Infraclass','Superorder','Order','Suborder',
  'Infraorder','Superfamily','Family','Subfamily','Tribe','Subtribe','Genus','Subgenus',
  'Section','Subsection','Species','Subspecies','Variety','Form','Subvariety','Race',
  'Stirp','Morph','Aberration','Subform','Unspecified'}

  \item{...}{Further args passed on to
  \code{itis_downstream} or \code{col_downstream}}
}
\value{
A named list of data.frames with the downstream names of
every supplied taxa. You get an NA if there was no match in
the database.
}
\description{
This function uses a while loop to continually collect
children taxa down to the taxonomic rank that you specify
in the \code{downto} parameter. You can get data from ITIS
(itis) or Catalogue of Life (col). There is no method
exposed by itis or col for getting taxa at a specific
taxonomic rank, so we do it ourselves inside the function.
}
\examples{
\dontrun{
# Plug in taxon names
downstream("Insecta", db = 'col', downto = 'Order')
downstream("Apis", db = 'col', downto = 'Species')

# Plug in IDs
id <- get_colid("Apis")
downstream(id, downto = 'Species')

## Equivalently, plug in the call to get the id via e.g., get_colid into downstream
identical(downstream(id, downto = 'Species'),
         downstream(get_colid("Apis"), downto = 'Species'))

id <- get_colid("Apis")
downstream(id, downto = 'Species')
downstream(get_colid("Apis"), downto = 'Species')

# Many taxa
sp <- names_list("genus", 3)
downstream(sp, db = 'col', downto = 'Species')
downstream(sp, db = 'itis', downto = 'Species')

# Both data sources
ids <- get_ids("Apis", db = c('col','itis'))
downstream(ids, downto = 'Species')
## same result
downstream(get_ids("Apis", db = c('col','itis')), downto = 'Species')
}
}

