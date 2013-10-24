\name{eol_search}
\alias{eol_search}
\title{Search for terms in EOL database.}
\usage{
  eol_search(terms, page = 1, exact = NULL,
    filter_tid = NULL, filter_heid = NULL,
    filter_by_string = NULL, cache_ttl = NULL,
    returntype = "data.frame", key = NULL,
    callopts = list())
}
\arguments{
  \item{terms}{search terms (character)}

  \item{page}{A maximum of 30 results are returned per
  page. This parameter allows you to fetch more pages of
  results if there are more than 30 matches (Default 1)}

  \item{exact}{Will find taxon pages if the preferred name
  or any synonym or common name exactly matches the search
  term.}

  \item{filter_tid}{Given an EOL page ID, search results
  will be limited to members of that taxonomic group}

  \item{filter_heid}{Given a Hierarchy Entry ID, search
  results will be limited to members of that taxonomic
  group}

  \item{filter_by_string}{Given a search term, an exact
  search will be made and that matching page will be used
  as the taxonomic group against which to filter search
  results}

  \item{cache_ttl}{The number of seconds you wish to have
  the response cached.}

  \item{returntype}{one of "list" of "data.frame"
  (character)}

  \item{key}{Your EOL API key; loads from .Rprofile.}

  \item{callopts}{Further args passed on to GET.}
}
\value{
  JSON list object, or data.frame.
}
\description{
  Search for terms in EOL database.
}
\details{
  It's possible to return JSON or XML with the EOL API.
  However, this function only returns JSON for now.
}
\examples{
\dontrun{
eol_search('Homo')
eol_search(terms='Salix')
eol_search(terms='Ursus americanus luteolus')
}
}

