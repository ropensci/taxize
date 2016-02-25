next_best_taxon <- function(x){
  paste("below-",
        tail(x[, "rank"][!duplicated(x[, "rank"])], n = 1
        ), sep = "")
}

lowest_common <- function(...){
  UseMethod("lowest_common")
}

lowest_common.default <- function(ids, db = NULL, class_list = NULL, ...) {
  if(!is.list(class_list)){
    class_list <- classification(ids, db = db, ...)
  }
  lc_helper(ids, class_list, ...)
}

lowest_common.uid <- function(ids, class_list = NULL, ...) {
  if(!is.list(class_list)){
    class_list <- classification(ids, db = "uid",  ...)
  }
  lc_helper(ids, class_list, ...)
}

lowest_common.tsn <- function(ids, class_list = NULL, ...) {
  if(!is.list(class_list)){
    class_list <- classification(ids, db = "itis", ...)
  }
  lc_helper(ids, class_list, ...)
}

lowest_common.gbifid <- function(ids, class_list = NULL, ...) {
  if(!is.list(class_list)){
    class_list <- classification(ids, db = "gbif", ...)
  }
  lc_helper(ids, class_list, ...)
}

lc_helper <- function(ids, class_list, low_rank = NULL) {
  idsc <- class_list[ids]
  cseq <- vapply(idsc, function(x) x[1, 1] != "unclassified sequences", logical(1))
  idsc <- idsc[cseq]
  if(is.null(low_rank)){
    x_row <- length(Reduce(intersect, lapply(idsc, "[[", 1)))
    x <- idsc[[1]][x_row, ]
    if (x[1, "rank"] == "no rank") {
      x[1, "rank"] <- next_best_taxon(idsc[[1]][1:x_row, ])
    }
    return(x)
  } else {
    # could test, warn/error that supplied rank is valid
    low_rank_names <- as.character(unique(unlist(lapply(idsc, function(x) x$name[which(x$rank == low_rank)]))))
    if(length(low_rank_names) == 1){
      return(low_rank_names)
    } else {
      return(NA)
    }
  }
}

# ids <- c("9031", "9823", "9606", "9470")
# id_class <- classification(ids, db = 'ncbi')
# lowest_common(ids[2:4], db = "ncbi")
# lowest_common(ids[2:4], db = "ncbi", low_rank = 'class')
# lowest_common(ids[2:4], db = "ncbi", low_rank = 'family')
# lowest_common(ids[2:4], class_list = id_class)
# lowest_common(ids[2:4], class_list = id_class, low_rank = 'class')
# lowest_common(ids[2:4], class_list = id_class, low_rank = 'family')
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
# 
# cool_orchids <- c("Angraecum sesquipedale", "Dracula vampira", "Masdevallia coccinea")
# orchid_ncbi <- get_uid(cool_orchids)
# orchid_gbif <- get_gbifid(cool_orchids)
# orchid_itis <- get_tsn(cool_orchids) # fails for 2/3
# 
# orchid_hier_ncbi <- classification(orchid_ncbi, db = 'ncbi')
# orchid_hier_gbif <- classification(orchid_gbif, db = 'gbif')
# orchid_hier_itis <- classification(orchid_itis, db = 'itis')
# 
# lowest_common(ids = get_uid(cool_orchids), low_rank = 'class')
# lowest_common(ids = get_uid(cool_orchids), low_rank = 'family')
# 
# lowest_common(ids = orchid_ncbi, class_list = orchid_hier_ncbi, low_rank = 'subfamily')
# lowest_common(ids = orchid_gbif, class_list = orchid_hier_gbif, low_rank = 'subfamily')
# 
# lowest_common(orchid_itis, db = "itis") # gives error from vapply
# lowest_common(get_tsn(cool_orchids)) # gives error from vapply
