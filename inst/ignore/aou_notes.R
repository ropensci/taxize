#  from https://www.birdpop.org/pages/birdSpeciesCodes.php
download.file("http://www.birdpop.org/docs/misc/IBPAOU.zip", destfile = "~/IBPAOU.zip")
unzip("~/IBPAOU.zip", exdir = "~/")
res <- read.csv("~/IBP-Alpha-Codes20.csv")
head(res)

# checklist from http://checklist.aou.org/taxa/
chklst <- read.csv('http://checklist.aou.org/taxa.csv', header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(chklst)

pryr::object_size(res)
pryr::object_size(chklst)
