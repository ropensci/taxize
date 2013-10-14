\name{tpl_families}
\alias{tpl_families}
\title{Get The Plant List families.}
\usage{
  tpl_families()
}
\arguments{
  \item{dir_}{Directory to write csv files to.}

  \item{family}{If you want just one, or >1 family, but not
  all, list them in a vector.}
}
\value{
  Returns nothing to console, except a message and progress
  bar. Writes csv files to dir_.
}
\description{
  Get The Plant List families.
}
\details{
  Throws a warning if you already have a directory of the
  one provided, but still works. Writes to your home
  directory, change dir_ as needed.
}
\examples{
\donttest{
# Get a data.frame of plant families, with the group name (Angiosperms, etc.)
head( tpl_families() )
}
}
\author{
  John Baumgartner (johnbb@student.unimelb.edu.au)
}
\seealso{
  \code{\link{tpl_search}} \code{\link{tpl_get}}
}

