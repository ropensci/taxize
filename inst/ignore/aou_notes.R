#  from http://www.birdpop.org/alphacodes.htm
download.file("http://www.birdpop.org/DownloadDocuments/LIST14.zip", destfile = "~/LIST14.zip")
unzip("~/LIST14.zip", exdir = "~/")
library(foreign)
res <- foreign::read.dbf("~/LIST14.DBF", as.is = TRUE)
head(res)

# checklist from http://checklist.aou.org/taxa/
chklst <- read.csv('http://checklist.aou.org/taxa.csv', header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(chklst)

pryr::object_size(res)
pryr::object_size(chklst)
