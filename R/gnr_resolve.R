#' Resolve names using Global Names Resolver.
#' 
#' Uses the Global Names Index, see \url{http://gni.globalnames.org/}.
#' 
#' @import stringr plyr httr
#' @param names character; taxonomic names to be resolved.
#' @param data_source_ids character; IDs to specify what data source
#'     is searched. See \code{\link[taxize]{gnr_datasources}}.
#' @param resolve_once logical; Find the first available match instead of 
#'    matches across all data sources with all possible renderings of a name. 
#'    When \code{TRUE}, response is rapid but incomplete.
#' @param with_context logical; Reduce the likelihood of matches to taxonomic 
#'    homonyms. When \code{TRUE} a common taxonomic context is calculated for 
#'    all supplied names from matches in data sources that have classification 
#'    tree paths. Names out of determined context are penalized during score 
#'    calculation.
#' @param stripauthority logical; If \code{TRUE}, gives back names with 
#'    taxonomic authorities. If \code{FALSE}, strips author names.
#' @param highestscore logical; Return those names with the highest score for 
#'    each searched name?
#' @param http The HTTP method to use, one of "get" or "post". Default="get". 
#'    Use http="post" with large queries. 
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return A data.frame.
#' @seealso \code{\link[taxize]{gnr_datasources}}
#' @export
#' @keywords resolve names taxonomy
#' @examples \dontrun{
#' gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"))
#' gnr_resolve(names = c("Asteraceae", "Plantae"))
#' 
#' # Using data source 12 (Encyclopedia of Life)
#' sources<- gnr_datasources()
#' sources
#' eol <- sources$id[sources$title == 'EOL']
#' gnr_resolve(names=c("Helianthos annuus","Homo sapians"), data_source_ids=eol)
#' }
gnr_resolve <- function(names, data_source_ids = NULL, resolve_once = FALSE, 
                        with_context = FALSE, stripauthority = FALSE, 
                        highestscore = TRUE, http="get")
{
  num = NULL
  url <- "http://resolver.globalnames.org/name_resolvers.json"
  names2 <- paste0(names, collapse = "|")
  args <- compact(list(names=names2, data_source_ids=data_source_ids, resolve_once=resolve_once, 
                       with_context=with_context))
  
  if(http=='get'){
    dat <- content(GET(url, query=args))$data
  } else
    if(http=='post'){
      args <- args[!names(args) %in% "names"]
      if(length(names) > 499){
        tt <- data.frame(num = 1:length(names), names=names)
        tt <- data.frame(ddply(tt, .(num), summarise, paste0(num,"|",names))[,2])
        write.table(tt, file="~/gnr_names.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)
        ss <- content(POST(url, query=args, body=list(file = upload_file(path="~/gnr_names.txt"))))
        bb <- "working"
        while(bb == "working"){
          temp <- content(GET(ss$url))
          bb <- temp$status
        }
        dat <- temp$data 
      } else
      {
        dat <- content(POST(url, query=args, body=list(names = names2)))$data
      }
    } else
      stop("http must be one of 'get' or 'post'")

  data_ <- llply(dat, function(y) list(y[["supplied_name_string"]], 
                                        llply(y$results, function(x) data.frame(x[c("name_string", "data_source_title", "score", "canonical_form")]))))
  data_2 <- ldply(data_, function(x) data.frame(x[[1]], ldply(x[[2]])))
  
  names(data_2)[c(1,2,5)] <- c("submitted_name", "matched_name", "matched_name2")
  out <- data_2[order(data_2$submitted_name), ]
  
  if(stripauthority){
    out <- out[ , !names(out) %in% "matched_name"]
  } else {
    out <- out[ , !names(out) %in% "matched_name2"]
  }        
  return(out)
}