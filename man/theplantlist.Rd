\name{theplantlist}
\alias{theplantlist}
\title{Get The Plant List csv files.}
\usage{
  theplantlist(dir_, family = NULL)
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
  [The Plant List](http://www.theplantlist.org/). Note that
  there is now a package on CRAN
  ([taxonstand](http://cran.r-project.org/web/packages/Taxonstand/))
  that uses only theplantlist.org to search plant names.
}
\details{
  Throws a warning if you already have a directory of the
  one provided, but still works.
}
\examples{
\dontrun{
theplantlist(dir_ = "~/mycsvilesdir") # writes to your home directory, change to where you want
theplantlist(dir_ = "~/myfiles", family = c("Platanaceae","Winteraceae")) # just a few families
}

