#  from http://www.birdpop.org/alphacodes.htm
download.file("http://www.birdpop.org/docs/misc/List18.zip", destfile = "~/List18.zip")
unzip("~/List18.zip", exdir = "~/")
library(foreign)
res <- foreign::read.dbf("~/List18.DBF", as.is = TRUE)
head(res)

# checklist from http://checklist.aou.org/taxa/
chklst <- read.csv('http://checklist.aou.org/taxa.csv', header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(chklst)

pryr::object_size(res)
pryr::object_size(chklst)
