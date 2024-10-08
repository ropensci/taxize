% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxize-package.R
\docType{package}
\name{taxize-package}
\alias{taxize-package}
\alias{taxize}
\title{Taxonomic Information from Around the Web}
\description{
This package interacts with a suite of web 'APIs'
for taxonomic tasks, such as verifying species names, getting
taxonomic hierarchies, and verifying name spelling.
}
\section{About}{

Allows users to search over many websites for species names (scientific and
common) and download up- and downstream taxonomic hierarchical information -
and many other things.

The functions in the package that hit a specific API have a prefix and suffix
separated by an underscore. They follow the format of \code{service_whatitdoes}.
For example, \code{gnr_resolve} uses the Global Names Resolver API to resolve
species names.

General functions in the package that don't hit a specific API don't have
two words separated by an underscore, e.g., \code{classification}

You need API keys for some data sources. See \link{taxize-authentication}
for more information.
}

\section{Currently supported APIs}{


\tabular{llc}{
API \tab prefix \tab SOAP? \cr
Encyclopedia of Life (EOL)  \tab eol \tab FALSE \cr
Integrated Taxonomic Information Service (ITIS)  \tab itis \tab FALSE \cr
Global Names Resolver (from EOL/GBIF) \tab gnr \tab FALSE \cr
Global Names Index (from EOL/GBIF) \tab gni \tab FALSE \cr
IUCN Red List \tab iucn \tab FALSE \cr
Tropicos (from Missouri Botanical Garden) \tab tp \tab FALSE \cr
Theplantlist.org \tab tpl \tab FALSE \cr
National Center for Biotechnology Information \tab ncbi \tab FALSE \cr
CANADENSYS Vascan name search API \tab vascan \tab FALSE \cr
International Plant Names Index (IPNI) \tab ipni \tab FALSE \cr
World Register of Marine Species (WoRMS) \tab worms \tab TRUE \cr
Barcode of Life Data Systems (BOLD) \tab bold \tab FALSE \cr
Pan-European Species directories Infrastructure (PESI) \tab pesi \tab TRUE \cr
Mycobank \tab myco \tab TRUE \cr
National Biodiversity Network (UK)	\tab nbn \tab FALSE \cr
Index Fungorum \tab fg \tab FALSE \cr
EU BON \tab eubon \tab FALSE \cr
Index of Names (ION) \tab ion \tab FALSE \cr
Open Tree of Life (TOL) \tab tol \tab FALSE \cr
World Register of Marine Species (WoRMS) \tab worms \tab FALSE \cr
NatureServe \tab natserv \tab FALSE \cr
}

If the source above has a \code{TRUE} in the \verb{SOAP?} column, it is not available
in this package. They are available from a different package called \strong{taxizesoap}.
See the GitHub repo for how to install https://github.com/ropensci/taxizesoap
}

\section{Catalogue of Life (COL)}{

COL introduced rate limiting recently in 2019 - which has made the API
essentially unusable - CoL+ is coming soon and we'll incorporate it here
when it's stable. See https://github.com/ropensci/colpluz for the
R implementation for CoL+
}

\seealso{
Useful links:
\itemize{
  \item \url{https://docs.ropensci.org/taxize/ (website)}
  \item \url{https://github.com/ropensci/taxize (devel)}
  \item \url{https://taxize.dev (user manual)}
  \item Report bugs at \url{https://github.com/ropensci/taxize/issues}
}

}
\author{
Scott Chamberlain

Eduard Szoecs \email{eduardszoecs@gmail.com}

Zachary Foster \email{zacharyfoster1989@gmail.com}

Carl Boettiger \email{cboettig@gmail.com}

Karthik Ram \email{karthik@ropensci.org}

Ignasi Bartomeus \email{nacho.bartomeus@gmail.com}

John Baumgartner \email{johnbb@student.unimelb.edu.au}

James O'Donnell \email{jodonnellbio@gmail.com}
}
\keyword{package}
