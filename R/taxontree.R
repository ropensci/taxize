# install_github('taxize_', 'ropensci')
library(taxize); library(XML); require(RCurl); library(ape); library(ritis)
mytaxa <- c("Pan troglodytes", "Drosophila melanogaster", "Homo sapiens")
temp <- classification(get_tsn(mytaxa, 'sciname'))
temp2 <- ldply(temp, function(x) data.frame(x, target = rep(x[nrow(x),"taxon"], nrow(x))))

rankorder <- getranknames()[,-1] # gets order of rank names from ritis package
rankorder <- ddply(rankorder, .(rankName), summarise, rankId=unique(rankId)) # gets unique ranks
temp3 <- merge(temp2, rankorder, by.x="rank", by.y="rankName", all.y=F) # merges rank order to temp2
temp3$rankId <- as.numeric(as.character(temp3$rankId)) # converts rankId to a numeric class

mm <- temp3[order(temp3$taxon), ] # sort by taxon 
summ <- ddply(mm, .(taxon), summarise, length(taxon)) # get list of number of taxon rows
oo <- mm[!mm$taxon %in% summ[summ$..1 == length(temp), "taxon"], ] # remove the taxon that is in all taxa
pp <- oo[order(oo$rankId),] # sort by rankId
qq <- ddply(pp, .(rankId), summarise, # get records of each taxon and target for each rankId
			taxon=length(unique(taxon)),
			target=length(unique(target)))
first <- max(qq[qq$target - qq$taxon == 1, "rankId"]) # get lowest taxonomic rank that has two of mytaxa
rr <- pp[pp$rankId == first, ] # get that set from pp
ss <- ddply(rr, .(taxon), summarise, length(taxon)) # get the name to extract
clade1 <- as.character(rr[rr$taxon == ss[ss$..1	== qq[qq$rankId == first,"taxon"], "taxon"], "target"]) # get those of my taxa that are a cloade
rem <- mytaxa[!mytaxa %in% clade1] # get the remainder taxon from mytaxa

# make a tree
tree <- paste("(", paste(rem,",",sep=""), paste("(", paste(clade1, collapse=","), ")", sep=""), ");", sep="")
tree <- str_replace_all(tree, "\\s", "_")
plot(read.tree(text=tree), no.margin=T)