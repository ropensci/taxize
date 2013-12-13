\docType{package}
\name{taxize-package}
\alias{taxize}
\alias{taxize-package}
\title{Taxnomic search and phylogeny retrieval.}
\description{
Taxnomic search and phylogeny retrieval.
}
\details{
We are developing taxize as a package to allow users to
search over many websites for species names (scientific and
common) and download up- and downstream taxonomic
hierarchical information - and many other things.

The functions in the package that hit a specific API have a
prefix and suffix separated by an underscore. They follow
the format of service_whatitdoes. For example, gnr_resolve
uses the Global Names Resolver API to resolve species
names. General functions in the package that don't hit a
specific API don't have two words separated by an
underscore, e.g., classification.

You need API keys for Encyclopedia of Life (EOL), the
Universal Biological Indexer and Organizer (uBio),
Tropicos, and Plantminer.

Currently supported APIs are:

\tabular{ll}{ API \tab prefix \cr Encyclopedia of Life
(EOL) \tab eol_ \cr Integrated Taxonomic Information
Service (ITIS) \tab itis_ \cr Phylomatic \tab phylomatic_
\cr uBio \tab ubio_ \cr Global Names Resolver (from
EOL/GBIF) \tab gnr_ \cr Global Names Index (from EOL/GBIF)
\tab gni_ \cr IUCN Red List \tab iucn_ \cr Tropicos (from
Missouri Botanical Garden) \tab tp_ \cr Plantminer \tab
plantminer_ \cr Theplantlist.org \tab tpl_ \cr Catalogue of
Life \tab col_ \cr Global Invasive Species Database \tab
gisd_ \cr }
}
\author{
Scott Chamberlain \email{myrmecocystus@gmail.com}

Eduard Szoecs \email{eduardszoecs@gmail.com}

Carl Boettiger \email{cboettig@gmail.com}
}
\keyword{package}

