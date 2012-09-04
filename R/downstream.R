#' Retrieve all taxa names or TSNs downstream in hierarchy from given TSN.
#' 
#' @import XML RCurl ritis plyr
#' @param searchtsn Quoted TSN for a taxonomic group (character).
#' @param outtaxlevel If not specified, function returns only direct children. 
#'    Other options = any quoted taxonomic level below that specified in searchtsn,
#'    e.g., 'genus' if searchtsn specifies a family
#' @param attachrank Should rank be attached to the output? (default=TRUE).
#' @return Names or TSNs of all downstream taxa in a data.frame.
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @examples \dontrun{
#' dat2 <- downstream(searchtsn = 180415, outtaxlevel = 'Species') # the superclass Osteichthyes
#' }
#' @export
downstream <- function(searchtsn = NA, attachrank = TRUE,
  outtaxlevel = c('Kingdom','Subkingdom','Phylum','Subphylum','Superclass',
    'Class','Subclass','Infraclass','Superorder','Order','Suborder',
    'Infraorder','Superfamily','Family','Subfamily','Tribe','Subtribe','Genus',
    'Subgenus','Species','Subspecies','Variety','Infrakingdom','Division',
    'Subdivision','Infradivision','Section','Subsection','Subvariety','Form',
    'Subform','Race','Stirp','Morph','Aberration','Unspecified'))
{
  dat <- gethierarchydownfromtsn(searchtsn)
  output <- list()
  ranks <- 1
  while(ranks > 0){
    dat_split <- split(dat, row.names(dat)) # make a list of each row
    addrank <- function(x) {
      x$rankName <- gettaxonomicranknamefromtsn(x$tsn)$rankName
      x
    }
    dat_split_ <- llply(dat_split, addrank)
    
    dat_matchedtaxlevel <- list()
    dat_notmatchedtaxlevel <- list()
    for(i in 1:length(dat_split_)) {
      if(dat_split_[[i]]$rankName %in% outtaxlevel){
        dat_matchedtaxlevel[[i]] <- dat_split_[[i]]
      } else
      { dat_notmatchedtaxlevel[[i]] <- dat_split_[[i]] }
    }  
    output <- dat_matchedtaxlevel # list contains rows matching outtaxlevel
    
    dat_notmatchedtaxlevel_df <- ldply(dat_notmatchedtaxlevel)
    dat <- ldply(dat_notmatchedtaxlevel_df$tsn, gethierarchydownfromtsn) # get hierarchy down
    ddd <- ldply(split(dat, row.names(dat)), addrank) # add rank name to each
    
    ranks <- unique(as.character(ddd$rankName))
    ranks <- ranks[!ranks %in% outtaxlevel]
  } 
  ldply(output)
}