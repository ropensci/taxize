\name{sci2comm}
\alias{sci2comm}
\alias{sci2comm.tsn}
\alias{sci2comm.uid}
\title{Get common names from scientific names.}
\usage{
  sci2comm(scinames, db = "eol", ...)

  \method{sci2comm}{uid} (id, ...)

  \method{sci2comm}{tsn} (id, ...)
}
\arguments{
  \item{scinames}{character; One or more scientific names
  or partial names.}

  \item{db}{character; Data source, one of \emph{"eol"}
  (default), \emph{"itis"} or \emph{"ncbi"}.}

  \item{...}{Further arguments passed on to functions
  \code{\link[taxize]{get_uid}} or
  \code{\link[taxize]{get_tsn}}.}

  \item{id}{character; identifiers, as returned by
  \code{\link[taxize]{get_tsn}} or
  \code{\link[taxize]{get_uid}}.}
}
\value{
  List of character - vectors.
}
\description{
  Get common names from scientific names.
}
\note{
  \emph{"ncbi"} and uid-method return common names from
  GenBank.
}
\examples{
\dontrun{
sci2comm(scinames='Helianthus annuus')
sci2comm(scinames='Helianthus annuus', db='itis')
sci2comm(scinames=c('black bear', 'roe deer'))
}
}
\author{
  Scott Chamberlain (myrmecocystus@gmail.com)
}
\seealso{
  \code{\link[taxize]{searchbycommonname}},
  \code{\link[taxize]{searchbycommonnamebeginswith}},
  \code{\link[taxize]{searchbycommonnameendswith}},
  \code{\link[taxize]{eol_search}},
  \code{\link[taxize]{tp_search}},
  \code{\link[taxize]{comm2sci}}
}

