\name{sci2comm}
\alias{sci2comm}
\title{Get common names from scientific names.}
\usage{
  sci2comm(scinames, db = "eol", ...)
}
\arguments{
  \item{scinames}{One or more scientific names or partial
  names.}

  \item{db}{Data source, one of eol (default) or itis}

  \item{...}{Further arguments passed on to functions}
}
\value{
  List of data.frame's.
}
\description{
  Get common names from scientific names.
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
  \code{\link[taxize]{tp_search}}

  \code{\link[taxize]{comm2sci}}
}

