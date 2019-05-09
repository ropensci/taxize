#' Get APG names
#'
#' Generic names and their replacements from the Angiosperm Phylogeny
#' Group III system of flowering plant classification.
#'
#' @param ... Curl args passed on to [crul::verb-GET]
#' @references <http://www.mobot.org/MOBOT/research/APweb/>
#' @name apg
#' @examples \dontrun{
#' head(apgOrders())
#' head(apgFamilies())
#' }

#' @export
#' @rdname apg
apgOrders <- function(...) {
  tt <- apg_GET("orders", ...)
  tmp <- strsplit(tt, "<font")[[1]]
  tmp2 <- strsplit(tmp[length(tmp)], "<br>")[[1]]
  tmp3 <- gsub("(<[^>]*>)|\r|\n|\\.$", "", tmp2)
  # remove stuff not cleaned up
  tmp4 <- tmp3[-grep("Back to", tmp3)]
  tmp5 <- sub(".*>", "", tmp4)
  # parse to a data.frame
  accorig <- acc <- grep("[Aa]ccepted", tmp5, value = TRUE)
  acc <- gsub("\\s.+", "", strtrim(acc))
  acc <- acc[nchar(acc) != 0]
  accorig <- accorig[nchar(accorig) != 0]
  accdf <- data.frame(order = acc, synonym = NA,
                      accepted = TRUE,
                      original = accorig, stringsAsFactors = FALSE)

  synorig <- syn <- grep("[Aa]ccepted", tmp5, invert = TRUE, value = TRUE)
  syn <- sapply(strsplit(syn, "=|\\s-"), strtrim)
  syn <- syn[vapply(syn, length, 1) != 0]
  synorig <- synorig[nchar(synorig) != 0]
  syndf <- rbind.fill(lapply(syn, function(x) {
    tmpdf <- rbind.data.frame(x)
    if (NCOL(tmpdf) == 2) {
      setNames(tmpdf, c("order", "synonym"))
    } else {
      setNames(tmpdf, c("order", "synonym", "comment"))
    }
  }))
  syndf$accepted <- FALSE
  syndf$original <- synorig

  rbind(accdf, syndf)
}

#' @export
#' @rdname apg
apgFamilies <- function(...) {
  tt <- apg_GET("families", ...)
  tmp <- strsplit(tt, "<font")[[1]]
  tmp2 <- strsplit(tmp[length(tmp)], "<br>|<br")[[1]]
  # look for any lines missing a <br> tag, and split
  tmp2 <- unlist(lapply(tmp2, function(z) {
    if (grepl("</a>\r", z)) {
      dd <- strsplit(z, "\r\n")[[1]]
      dd[nchar(dd) != 0]
    } else {
      z
    }
  }))
  # remove html tags
  tmp3 <- gsub("(<[^>]*>)|\r|\n|\\.$", "", tmp2)
  # remove stuff not cleaned up
  tmp4 <- tmp3[-grep("Back to", tmp3)]
  tmp5 <- sub(".*>", "", tmp4)
  # parse to a data.frame
  syn <- sapply(strsplit(tmp5, "=|\\s-"), strtrim)
  syn <- syn[vapply(syn, length, 1) != 0]
  synorig <- tmp5[nchar(tmp5) != 0]
  syndf <- rbind.fill(lapply(syn, function(x) {
    tmpdf <- rbind.data.frame(x)
    if (NCOL(tmpdf) == 2) {
      setNames(tmpdf, c("family", "order"))
    } else {
      setNames(tmpdf, c("family", "synonym", "order"))
    }
  }))
  syndf[] <- lapply(syndf, as.character)
  syndf$accepted <- syndf$synonym
  syndf$accepted[is.na(syndf$accepted)] <- TRUE
  syndf$accepted[ syndf$accepted != TRUE ] <- FALSE
  syndf$accepted <- as.logical(syndf$accepted)
  syndf$original <- synorig
  syndf
}

apg_GET <- function(x, ...) {
  cli <- crul::HttpClient$new(apg_base(),
    headers = tx_ual, opts = list(...))
  res <- cli$get(sprintf("MOBOT/research/APweb/top/synonymy%s.html", x))
  res$raise_for_status()
  res$parse("UTF-8")
}

apg_base <- function() "http://www.mobot.org"
