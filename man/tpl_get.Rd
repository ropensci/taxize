\name{tpl_get}
\alias{tpl_get}
\title{Get The Plant List csv files.}
\usage{
  tpl_get(dir_, family = NULL)
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
  The Plant List \url{http://www.theplantlist.org/}. Note
  that there is now a package on CRAN (taxonstand -
  \url{http://cran.r-project.org/web/packages/Taxonstand/})
  that uses only theplantlist.org to search plant names.
}
\details{
  Throws a warning if you already have a directory of the
  one provided, but still works.
}
\examples{
\donttest{
# writes to your home directory, change to where you want
tpl_get(dir_ = "~/foo")

# just a few families
tpl_get(dir_ = "~/foo2", family = c("Platanaceae","Winteraceae"))
}
}

