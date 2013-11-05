\name{eol_invasive}
\alias{eol_invasive}
\title{Search for terms in EOL database.}
\usage{
  eol_invasive(name = NULL, dataset = "all", page = NULL,
    per_page = NULL, key = NULL, callopts = list())
}
\arguments{
  \item{name}{A taxonomic name, or a vector of names.}

  \item{dataset}{One of all, gisd100, gisd, gris, isc,
  daisie, i3n, or mineps.  See the Details for what each
  dataset ID.}

  \item{page}{A maximum of 30 results are returned per
  page. This parameter allows you to fetch more pages of
  results if there are more than 30 matches (Default 1)}

  \item{per_page}{Results to get per page}

  \item{key}{Your EOL API key; loads from .Rprofile.}

  \item{callopts}{Further args passed on to GET.}
}
\value{
  A list of data.frame's/strings with results, with each
  element named by the input elements to the name
  parameter.
}
\description{
  If a two-column data.frame is returned with the species
  you searched for, your species is listead in the database
  you searched. If your searched taxon is not found, we
  return a string "<your species name> was not found".
}
\details{
  Beware that some datasets are quite large, and may take
  30 sec to a minute to pull down all data before we can
  search for your species. Note there is no parameter in
  this API method for searching by taxon name.

  This function is vectorized, so you can pass a single
  name or a vector of names.

  It's possible to return JSON or XML with the EOL API.
  However, this function only returns JSON for now.

  Options for the dataset parameter are \itemize{ \item all
  - All datasets \item gisd100 - 100 of the World's Worst
  Invasive Alien Species (Global Invasive Species Database)
  \item gisd - Global Invasive Species Database 2013 \item
  gris - Global Register of Invasive Species (GRIS) Taxa
  \item isc - Centre for Agriculture and Biosciences
  International Invasive Species Compendium (ISC) \item
  daisie - Delivering Alien Invasive Species Inventories
  for Europe (DAISIE) Species List \item i3n - IABIN
  Invasives Information Network (I3N) Species \item mineps
  - Marine Invaders of the NE Pacific Species }
}
\examples{
\dontrun{
eol_invasive(name='Brassica oleracea', dataset='gisd')
eol_invasive(name='Sargassum', dataset='gisd')
eol_invasive(name='Ciona intestinalis', dataset='mineps')
eol_invasive(name='Rhinella marina', dataset='gris')
eol_invasive(name=c('Rhinella marina','Sturnus vulgaris','Cygnus olor','Pinus concolor'), dataset='gris')
eol_invasive(name=c('Lymantria dispar','Cygnus olor','Hydrilla verticillata','Pinus concolor'), dataset='i3n')
eol_invasive(name=c('Branta canadensis','Gallus gallus','Myiopsitta monachus'), dataset='daisie')
eol_invasive(name=c('Branta canadensis','Gallus gallus','Myiopsitta monachus'), dataset='isc')
}
}
\references{
  See info for each data source at
  \url{http://eol.org/collections/55367/taxa}
}

