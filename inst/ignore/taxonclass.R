out <- make_taxon(genus="Poa", epithet="annua", authority="L.")
out <- make_taxon(genus="Poa", epithet="annua", authority="L.", family='Poaceae', clazz='Poales', kingdom='Plantae', variety='annua')
out@binomial
out@binomial@canonical
out@binomial@species
out@binomial@authority
out@classification
out@classification@family
out[['family']] # get a single rank
out['family','kingdom'] # get a range of ranks
out['variety','genus']
gethier(out) # get hierarchy as data.frame

make_taxon <- function(genus, epithet, authority, ...){
  res <- new("binomial", 
             genus=genus, epithet=epithet, canonical=paste(genus,epithet,collapse=" "), 
             species=paste(genus,epithet,authority,collapse=" "), authority=authority)
  hier <- new("classification", genus=genus, species=paste(genus,epithet,collapse=" "), ...)
  new("taxon", binomial=res, classification=hier)
}

setClass("binomial", slots = c(genus="character", epithet="character", canonical="character", species="character", authority="character"))

setClass("classification", slots = c(
  superkingdom="character",
  kingdom="character",
  subkingdom="character",
  infrakingdom="character",
  division="character",
  phylum="character",
  subdivision="character",
  infradavision="character",
  superclass="character",
  clazz="character",
  subclass="character",
  infraclass="character",
  superorder="character",
  order="character",
  suborder="character",
  infraorder="character",
  superfamily="character",
  family="character",
  subfamily="character",
  tribe="character",
  subtribe="character",
  genus="character",
  subgenus="character",
  section="character",
  subsection="character",
  species="character",
  subspecies="character",
  variety="character", 
  race="character",
  subvariety="character",
  stirp="character",
  morph="character", 
  form="character", 
  aberration="character", 
  subform="character", 
  unspecified="character"
), prototype = prototype(superkingdom="",
                         kingdom="",
                         subkingdom="",
                         infrakingdom="",
                         division="",
                         phylum="",
                         subdivision="",
                         infradavision="",
                         superclass="",
                         clazz="",
                         subclass="",
                         infraclass="",
                         superorder="",
                         order="",
                         suborder="",
                         infraorder="",
                         superfamily="",
                         family="",
                         subfamily="",
                         tribe="",
                         subtribe="",
                         genus="",
                         subgenus="",
                         section="",
                         subsection="",
                         species="",
                         subspecies="",
                         variety="", 
                         race="",
                         subvariety="",
                         stirp="",
                         morph="", 
                         form="", 
                         aberration="", 
                         subform="", 
                         unspecified=""))
setClass("taxon", slots = c(binomial = 'binomial', classification = 'classification'))
# setClass("taxon", contains = c("binomial","classification"))

setMethod("[[", "taxon", function(x, i, ...){
  tmp <- x@classification
  slot(tmp, i)
})

setMethod("[", "taxon", function(x, i, j, ...){
  tmp <- x@classification
  nn <- slotNames(tmp)
  from <- match(j, nn)
  to <- match(i, nn)
  vapply(nn[to:from], function(g) slot(tmp, g), "")
})

setGeneric("gethier", function(x) standardGeneric("gethier"))
setMethod("gethier", "taxon", function(x){
  tmp <- x@classification
  nn <- slotNames(tmp)
  vals <- vapply(nn, function(g) slot(tmp, g), "", USE.NAMES = FALSE)
  data.frame(rank=nn, value=vals, stringsAsFactors = FALSE)
})