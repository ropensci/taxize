#' Hierarchy search
#' @export
#' @template worms_id
#' @examples \dontrun{
#' worms_hierarchy(ids=733271)
#' worms_hierarchy(ids=123080)
#' }
worms_hierarchy <- function(ids=NULL, opts=NULL, iface=NULL, ...)
{
  server <- 'http://www.marinespecies.org/aphia.php?p=soap'
  if(!is.null(iface)) worms_iface <- iface
  fxn <- worms_get_fxn('getAphiaClassificationByID')
  res <- fxn(AphiaID = ids, server = server, .opts = opts)
  df <- data.frame(aphiaid=res@AphiaID, rank=res@rank, scientificname=res@scientificname, stringsAsFactors = FALSE)
  hier <- slot(res, "child")
  rbind(df, parse_hier(hier, c("AphiaID","rank","scientificname")))
}

parse_hier <- function(x, slotnames){
  out <- list()
  iter <- 1
  done <- NULL
  xplus <- x
  while(is.null(done)){
    iter <- iter+1
    vals <- sapply(slotnames, function(x) slot(xplus, name = x))
    out[[iter]] <- vals
    xplus <- xplus@child
    done <- if(!length(xplus@AphiaID)==0) NULL else "done"
  }
  tmp <- ldply(compact(out))
  names(tmp)[1] <- 'aphiaid'
  tmp
}