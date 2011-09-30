# Function to match taxonomic names using the Taxonomic Name Resolution Service

tnrsmatch <- 
# Args:
#   retrieve: either 'best' or 'all' for returning the best matched or 
#     all names, respectively (character)
#   taxnames: quoted taxonomic names to search in a vector (character)
#   output: 'all' for raw list output or 'names' for matched names
#     and their match scores, plus plant family names (character)
# Examples:
# mynames <- c("shorea robusta", "pandanus patina", "oryza sativa", "durio zibethinus", "rubus ulmifolius", "asclepias curassavica", "pistacia lentiscus")
# tnrsmatch('best', mynames, 'names')
# tnrsmatch(retrieve = 'all', taxnames = c('helianthus annuus', 'acacia', 'saltea'), output = 'names')
# tnrsmatch(retrieve = 'all', taxnames = c('helianthus annuus', 'acacia', 'saltea'), output = 'all')
# tnrsmatch(retrieve = 'best', taxnames = c('helianthus annuus', 'acacia', 'saltea'), output = 'names')

function(retrieve = 'best', taxnames = NA, output = NA,
  url = "http://ohmsford.iplantc.org/tnrsm-svc/matchNames?retrieve=") {
  
  urlplus <- paste(url, retrieve, sep='')
  query <- function(x) {
    taxnames_ <- paste(str_replace_all(x, ' ', '%20'), collapse=',')
    namespart <- paste(urlplus, '&names=', taxnames_, sep='')
    fromJSON(namespart)
  }
  temp <- llply(taxnames, query)
  if (output == 'all') { return(temp) } else
    if (output == 'names') {
      getdf <- function(x) {
        outdf <- ldply(x[[1]], function(y) c(y$nameSubmitted, y$nameScientific, 
            y$family, round(as.numeric(y$overall), 2), y$acceptance))
        names(outdf) <- c('SubmitName','MatchName','MatchFam','MatchScore','Accept?')
        outdf    
      }
      ldply(temp, getdf)
    }
}