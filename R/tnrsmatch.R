# Function to match taxonomic names using the Taxonomic Name Resolution Service

tnrsmatch <- 
# Args:
#   retrieve: either 'best' or 'all' for returning the best matched or 
#     all names, respectively (character)
#   taxnames: taxonomic names to search (character)
#   output: 'all' for raw list output or 'names' for matched names
#     and their match scores, plus plant family names (character)
# Examples:
#   tnrsmatch('all', c('helianthus annuus', 'acacia', 'saltea'), 'names')
#   tnrsmatch(taxnames = c('helianthus annuus', 'acacia', 'saltea'), output = 'all')
#   tnrsmatch(taxnames = c('helianthus annuus', 'acacia', 'saltea'), output = 'names')

function(retrieve = 'best', taxnames, output = 'all',
  url = "http://tnrs.iplantc.org/tnrsm-svc/matchNames?retrieve=") {
  
  urlplus <- paste(url, retrieve, sep='')
  taxnames_ <- paste(str_replace_all(taxnames, ' ', '%20'), collapse=',')
  namespart <- paste(urlplus, '&names=', taxnames_, sep='')
  out <- fromJSON(namespart)
  if (output == 'all') { return(out) } else
    if (output == 'names') {
      out[[1]][[1]]$nameSubmitted  
      outdf <- ldply(out[[1]], function(x) c(x$nameSubmitted, x$nameScientific, 
          x$family, round(as.numeric(x$overall), 2), x$acceptance))
      names(outdf) <- c('SubmitName','MatchName','MatchFam','MatchScore','Accept?')
      return(outdf)
    }
}