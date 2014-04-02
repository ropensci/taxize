(out <- make_taxon(genus="Poa", epithet="annua", authority="L."))
(out <- make_taxon(genus="Poa", epithet="annua", authority="L.", 
                  family='Poaceae', clazz='Poales', kingdom='Plantae', variety='annua'))
(out <- make_taxon(genus="Poa"))
make_taxon(epithet="annua") # errors
out@binomial
out@binomial@canonical
out@binomial@species
out@binomial@authority
out@classification
out@classification@family
out[['family']] # get a single rank
out['family','kingdom'] # get a range of ranks
out['variety','genus'] # get a range of ranks
gethier(out) # get hierarchy as data.frame


df <- data.frame(rank=c('family','tribe','subtribe','genus','subgenus','species'), 
                 name=c('Helianthi','Helianthi','Helianthi','Poa','Festuci','Poa annua'), 
                 id=c(1,2,3,4,5,6),
                 stringsAsFactors = FALSE)

apply(df, 1, make_taxon_fromclass)

make_taxon_fromclass <- function(x, authority="none", ...){
  rank <- x[['rank']]
  name <- x[['name']]
  id <- as.numeric(x[['id']])
  res <- list(new("taxonref", rank=rank, name=name, id=id))
  names(res) <- rank
  res[1]
}

#   species <- x[ x$rank %in% 'species', 'name']
#   epithet <- strsplit(species, " ")[[1]][[2]]
#   res <- new("binomial", 
#              genus=genus, 
#              epithet=epithet, 
#              canonical=paste(genus,epithet,collapse=" "), 
#              species=paste(genus,epithet,authority,collapse=" "), 
#              authority=authority)
#   hier <- new("classification", 
#               genus=new("taxonref", name=genus), 
#               species=new("taxonref", name=paste(genus,epithet,collapse=" ")))
#   if(length(input) > 0){
#     output <- list()
#     for(i in seq_along(input)){
#       slot(hier, names(input)[i]) <- new("taxonref", name=input[[i]])
#     }
#   }
#   new("taxon", binomial=res, classification=hier)
# }

make_taxon <- function(genus="none", epithet="none", authority="none", ...){
  if(genus=='none') stop("You must supply at least genus")
  res <- new("binomial", 
             genus=genus, 
             epithet=epithet, 
             canonical=paste(genus,epithet,collapse=" "), 
             species=paste(genus,epithet,authority,collapse=" "), 
             authority=authority)
  input <- list(...)
  hier <- new("classification", 
              genus=new("taxonref", name=genus), 
              species=new("taxonref", name=paste(genus,epithet,collapse=" ")))
  if(length(input) > 0){
    output <- list()
    for(i in seq_along(input)){
      slot(hier, names(input)[i]) <- new("taxonref", name=input[[i]])
    }
  }
  new("taxon", binomial=res, classification=hier)
}

setClass("binomial", slots = c(genus="character", epithet="character", canonical="character", species="character", authority="character"))

setClass("taxonref", slots = c(rank='character', name='character', id='numeric', source='character'), 
         prototype = prototype(rank='none', name='none', id=NaN, source='none'))

setClass("ListOfTaxonRefs", slots = c(names="character"), contains="list")

setAs("character", "taxonref", function(from) new("taxonref", name=from))

# setMethod("combinetaxonref", "ListOfTaxonRefs", function(...){
#   input <- list(...)
#   assert_that(all(sapply(input, is, "ListOfTaxonRefs")))
#   
# })

# new("ListOfTaxonRefs", lapply(list(kingdom="adfa", family="adf"), as, "taxonref"))

setClass("classification", slots = c(
  kingdom="taxonref",
  subkingdom="taxonref",
  infrakingdom="taxonref",
  division="taxonref",
  phylum="taxonref",
  subdivision="taxonref",
  infradavision="taxonref",
  superclass="taxonref",
  clazz="taxonref",
  subclass="taxonref",
  infraclass="taxonref",
  superorder="taxonref",
  order="taxonref",
  suborder="taxonref",
  infraorder="taxonref",
  superfamily="taxonref",
  family="taxonref",
  subfamily="taxonref",
  tribe="taxonref",
  subtribe="taxonref",
  genus="taxonref",
  subgenus="taxonref",
  section="taxonref",
  subsection="taxonref",
  species="taxonref",
  subspecies="taxonref",
  variety="taxonref", 
  race="taxonref",
  subvariety="taxonref",
  stirp="taxonref",
  morph="taxonref", 
  form="taxonref", 
  aberration="taxonref", 
  subform="taxonref", 
  unspecified="taxonref"
), contains = "list")
# , contains = "list")
setClass("taxon", slots = c(binomial = 'binomial', classification = 'classification'))
setClass("ListOfTaxa", slots = c(taxon = 'taxon'), prototype = prototype(list()), contains = 'list')
# 
# new('ListOfTaxa', taxon=out)
# out2 <- list(make_taxon(genus="Poa", epithet="annua", authority="L."), 
#              make_taxon(genus="Helianthus", epithet="annuus", authority="Baker"))
# gg <- new('ListOfTaxa', lapply(out2, as, "taxon"))
# class(gg)
# length(gg@.Data)

setMethod("[[", "taxon", function(x, i, ...){
  tmp <- x@classification
  slot(tmp, i)@name
})

setMethod("[", "taxon", function(x, i, j, ...){
  tmp <- x@classification
  nn <- slotNames(tmp)
  from <- match(j, nn)
  to <- match(i, nn)
  vapply(nn[to:from], function(g) slot(tmp, g)@name, "")
})

setGeneric("gethier", function(x) standardGeneric("gethier"))
setMethod("gethier", "taxon", function(x){
  tmp <- x@classification
  nn <- slotNames(tmp)[-1]
  vals <- vapply(nn, function(g) slot(tmp, g)@name, "", USE.NAMES = FALSE)
  data.frame(rank=nn, value=vals, stringsAsFactors = FALSE)
})



####
# subset data.frame using taxonomy
df <- data.frame(family=c('Asteraceae','Asteraceae','Asteraceae','Poaceae','Poaceae','Poaceae'), 
                 tribe=c('Helianthi','Helianthi','Helianthi','Poaeae','Festuci','Poaeae'), 
                 genus=c('Helianthus','Helianthus','Madia','Poa','Festuca','Holodiscus'), stringsAsFactors = FALSE)
df_tax <- new('taxonDataFrame', data=df)
df_tax['family','Asteraceae']
df_tax['genus','Madia']
df_tax['tribe','Helianthi']

setClass("taxonDataFrame", slots = c(data = 'data.frame'))
setMethod("[", "taxonDataFrame", function(x, i, j, ...){
  tmp <- x@data
  tmp[ tmp[[i]] %in% j, ]
})