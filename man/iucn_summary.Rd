\name{iucn_summary}
\alias{iucn_status.iucn}
\alias{iucn_summary}
\title{Get a summary from the IUCN Red List.}
\usage{
iucn_summary(sciname, silent = TRUE, parallel = FALSE, ...)

\method{iucn_status}{iucn}(x, ...)
}
\arguments{
  \item{sciname}{character; Scientific name. Should be
  cleand and in the format \emph{<Genus> <Species>}.}

  \item{silent}{logical; Make errors silent or not (when
  species not found).}

  \item{parallel}{logical; Search in parallel to speed up
  search. You have to register a parallel backend if
  \code{TRUE}. See e.g., doMC, doSNOW, etc.}

  \item{...}{Currently not used.}

  \item{x}{an \code{iucn} object as returned by
  \code{\link[taxize]{iucn_summary}}.}
}
\value{
A list (for every species one entry) of lists with the
following items: \item{status}{Red List Category.}
\item{history}{History of status, if available.}
\item{distr}{Geographic distribution, if available.}
\item{trend}{Trend of population size, if available.}
}
\description{
Get a summary from the IUCN Red List
(\url{http://www.iucnredlist.org/}).
}
\note{
Not all entries (history, distr, trend) are available for
every species and NA is returned.
\code{\link[taxize]{iucn_status}} is an extractor function
to easily extract status into a vector.
}
\examples{
\dontrun{
ia <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
ia <- iucn_summary(c("Panthera uncia", "Lynx lynx", "aaa"))
# extract status
iucn_status(ia)
# extract other available information
ia[['Lynx lynx']]$history
ia[['Panthera uncia']]$distr
ia[[2]]$trend
}
}
\author{
Eduard Szoecs, \email{eduardszoecs@gmail.com}
}
\seealso{
\code{\link[taxize]{iucn_status}}
}

