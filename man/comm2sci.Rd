\name{comm2sci}
\alias{comm2sci}
\title{Get scientific names from common names.}
\usage{
  comm2sci(commnames, db = "eol", itisby = "search", ...)
}
\arguments{
  \item{commnames}{One or more common names or partial
  names.}

  \item{db}{Data source, one of eol (default), itis, or
  tropicos}

  \item{itisby}{Search for common names across entire names
  (search, default), at beginning of names (begin), or at
  end of names (end).}

  \item{...}{Further arguments passed on to functions...}
}
\value{
  List of data.frame's.
}
\description{
  Get scientific names from common names.
}
\examples{
\dontrun{
comm2sci(commnames='black bear')
comm2sci(commnames='black bear', db='itis')
comm2sci(commnames='inch', db='itis', itisby='begin')
comm2sci(commnames='snake', db='itis', itisby='end')
comm2sci(commnames='annual blue grass', db='tropicos')
comm2sci(commnames=c('annual blue grass','tree of heaven'), db='tropicos')
do.call(rbind.fill, comm2sci(commnames=c('annual blue grass','tree of heaven'), db='tropicos'))
comm2sci(commnames=c('black bear', 'roe deer'))
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

  \code{\link[taxize]{sci2comm}}
}

