next_best_taxon <- function(x){
  paste("below-",
        tail(x[, "rank"][!duplicated(x[, "rank"])], n = 1
        ), sep = "")
}

lowest_common <- function(...){
  UseMethod("lowest_common")
}

lowest_common.default <- function(ids, db = NULL, ...) {
  class_list <- classification(ids, db = db, ...)
  lc_helper(ids, class_list)
}

lowest_common.uid <- function(ids, ...) {
  class_list <- classification(ids, db = "uid",  ...)
  lc_helper(ids, class_list)
}

lowest_common.tsn <- function(ids, ...) {
  class_list <- classification(ids, db = "itis", ...)
  lc_helper(ids, class_list)
}

lowest_common.gbifid <- function(ids, ...) {
  class_list <- classification(ids, db = "gbif", ...)
  lc_helper(ids, class_list)
}

lc_helper <- function(ids, class_list) {
  idsc <- class_list[ids]
  cseq <- vapply(idsc, function(x) x[1, 1] != "unclassified sequences", logical(1))
  idsc <- idsc[cseq]
  x_row <- length(Reduce(intersect, lapply(idsc, "[[", 1)))
  x <- idsc[[1]][x_row, ]
  if (x[1, "rank"] == "no rank") {
    x[1, "rank"] <- next_best_taxon(idsc[[1]][1:x_row, ])
  }
  return(x)
}

# ids <- c("9031", "9823", "9606", "9470")
# lowest_common(ids[2:4], db = "ncbi")
#
# spp <- c("Sus scrofa", "Homo sapiens", "Nycticebus coucang")
# lowest_common(spp, db = "ncbi")
# lowest_common(get_uid(spp))
#
# spp <- c("Sus scrofa", "Homo sapiens", "Nycticebus coucang")
# lowest_common(spp, db = "itis")
# lowest_common(get_tsn(spp))
#
#
# gbifids <- c("2704179", "3119195")
# lowest_common(gbifids, db = "gbif")
#
# spp <- c("Poa annua", "Helianthus annuus")
# lowest_common(get_gbifid(spp))
