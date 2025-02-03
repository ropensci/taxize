#' Convert a list of classifications to a tree.
#'
#' This function converts a list of hierarchies for individual species into
#' a single species by taxonomic level matrix, then calculates a distance
#' matrix based on taxonomy alone, and outputs either a phylo or dist object.
#' See details for more information.
#'
#' @export
#' @param input List of classification data.frame's from the function
#' [classification()]
#' @param varstep Vary step lengths between successive levels relative to
#' proportional loss of the number of distinct classes.
#' @param check	If TRUE, remove all redundant levels which are different for
#' all rows or constant for all rows and regard each row as a different basal
#' taxon (species). If FALSE all levels are retained and basal taxa (species)
#' also must be coded as variables (columns). You will get a warning if
#' species are not coded, but you can ignore this if that was your intention.
#' @param remove_shared If `TRUE`, remove any taxa that are coarser ranks
#'   present in other taxa, such as both a genus and a species in that genus in
#'   the same tree.
#' @param ... Further arguments passed on to hclust.
#' @param x Input object to print or plot - output from class2tree function.
#' @return An object of class "classtree" with slots:
#' 
#' * phylo - The resulting object, a phylo object
#' * classification - The classification data.frame, with taxa as rows,
#'  and different classification levels as columns
#' * distmat - Distance matrix
#' * names - The names of the tips of the phylogeny
#'
#' Note that when you execute the resulting object, you only get the phylo
#' object. You can get to the other 3 slots by calling them directly, like
#' output$names, etc.
#' @details See [vegan::taxa2dist()]. Thanks to Jari Oksanen for
#' making the taxa2dist function and pointing it out, and Clarke & Warwick
#' (1998, 2001), which taxa2dist was based on.
#' The taxonomy tree created is not only based on the clustering of the 
#' taxonomy ranks (e.g. strain, species, genus, ...), but it also utilizes the
#' actual taxon clades (e.g. mammals, plants or reptiles, etc.). The process of
#' this function is as following: First, all possible taxonomy ranks and their 
#' corresponding IDs for each given taxon will be collected from the input.
#' Then, the rank vectors of all taxa will be aligned, so that they together 
#' will become a matrix where columns are ordered taxonomy ranks of all taxa and
#' rows are the rank vectors of those taxa. After that, the rank matrix will be
#' converted into taxonomy ID matrix, any missing rank will have a pseudo
#' ID from the previous rank. Finally, this taxonomy ID matrix will be used to
#' cluster taxa that have similar taxonomy hierarchy together.
#' @examples \dontrun{
#' spnames <- c('Quercus robur', 'Iris oratoria', 'Arachis paraguariensis',
#'  'Helianthus annuus','Madia elegans','Lupinus albicaulis',
#'  'Pinus lambertiana')
#' out <- classification(spnames, db='itis')
#' tr <- class2tree(out)
#' plot(tr)
#'
#' spnames <- c('Klattia flava', 'Trollius sibiricus',
#'  'Arachis paraguariensis',
#'  'Tanacetum boreale', 'Gentiana yakushimensis','Sesamum schinzianum',
#'  'Pilea verrucosa','Tibouchina striphnocalyx','Lycium dasystemum',
#'  'Berkheya echinacea','Androcymbium villosum',
#'  'Helianthus annuus','Madia elegans','Lupinus albicaulis',
#'  'Pinus lambertiana')
#' out <- classification(spnames, db='ncbi')
#' tr <- class2tree(out)
#' plot(tr)
#' }
#' 
class2tree <- function(input, varstep = TRUE, check = TRUE, remove_shared = FALSE, ...) {
  
  if (any(is.na(input))) {
    message('Removed species without classification')
    input <- input[!is.na(input)]
  }
  

  # Check that there is more than 2 taxon
  if (length(input) < 3)
    stop("Your input list of classifications must be 3 or longer.")

  if (length(unique(names(input))) < length(names(input)))
    stop("Input list of classifications contains duplicates")

  # Convert tibbles to data.frames
  input <- lapply(input, as.data.frame)
  
  # Get rank and ID list
  message('Get all ranks and their taxIDs')
  rankList <- dt2df(lapply(input, get_rank), idcol = FALSE)
  nameList <- dt2df(lapply(input, get_name), idcol = FALSE)
  strainIndex <- grep("norank", rankList$X1)
  rankList$X1[strainIndex] <- "strain"
  nameList$X1[strainIndex] <- 
    gsub("norank_[[:digit:]]+", "strain", nameList$X1[strainIndex])

  # Create taxonomy matrix
  message('Align taxonomy hierarchies...')
  df <- taxonomy_table_creator(nameList, rankList, remove_shared = remove_shared)

  if (!inherits(df, "data.frame")) {
    stop("no taxon ranks in common - try different inputs")
  }
  message('Taxonomy alignment done!')

  row.names(df) <- df[,1]
  df <- df[,-1]

  # calculate distance matrix
  taxdis <- tryCatch(taxa2dist(df, varstep = varstep, check = check),
                     error = function(e) e)

  tdf <- t(df)
  for (i in 1:ncol(tdf)){
    tdf[,i][duplicated(tdf[,i])] <- NA
  }

  # check for incorrect dimensions error
  if (is(taxdis, 'simpleError'))
    stop("Try check=FALSE, but see docs for taxa2dist function in the vegan package for details.")
  message('Calculate distance matrix')
  out <- as.phylo.hclust(hclust(taxdis, ...))
  out <- ape::di2multi(out)
  # Add node labels
  message('Add node labels')
  node_ids <- sort(unique(out$edge[,1]))
  node_labels <- sapply(phangorn::Descendants(out, node_ids), function(x) {
    sub_df <- df[out$tip.label[x],]
    node_col_i <- which(sapply(1:ncol(sub_df), function(i) {
      length(unique(sub_df[,i]))==1
    }))[1]
    if (is.na(node_col_i)) {
      NA_character_
    } else {
      unique(sub_df[,node_col_i])
    }
  })
  out$node.label <- node_labels
  
  res <- list(phylo = out, classification = as.data.frame(t(tdf)), 
    distmat = taxdis, names = names(input))
  class(res) <- 'classtree'
  return( res )
}

#' @method plot classtree
#' @export
#' @rdname class2tree
plot.classtree <- function(x, ...) {
  if (!is(x$phylo, "phylo"))
    stop("Input object must have a slot in 'phylo' of class 'phylo'")
  plot(x$phylo, ...)
}

#' @method print classtree
#' @export
#' @rdname class2tree
print.classtree <- function(x, ...) {
  if (!is(x$phylo, "phylo"))
    stop("Input object must have a slot in 'phylo' of class 'phylo'")
  print(x$phylo)
}

# Function from the vegan package
# CRAN: http://cran.rstudio.com/web/packages/vegan/
# License: GPL-2
# Maintainer:	Jari Oksanen <jari.oksanen at oulu.fi>
taxa2dist <- function(x, varstep = FALSE, check = TRUE, labels) {
  rich <- apply(x, 2, function(taxa) length(unique(taxa)))
  S <- nrow(x)
  if (check) {
    keep <- rich < S & rich > 1
    rich <- rich[keep]
    x <- x[, keep]
  }
  i <- rev(order(rich))
  x <- x[, i]
  rich <- rich[i]
  if (varstep) {
    add <- -diff(c(nrow(x), rich, 1))
    add <- add/c(S, rich)
    add <- add/sum(add) * 100
  }
  else {
    add <- rep(100/(ncol(x) + check), ncol(x) + check)
  }
  if (!is.null(names(add)))
    names(add) <- c("Base", names(add)[-length(add)])
  if (!check)
    add <- c(0, add)
  out <- matrix(add[1], nrow(x), nrow(x))
  for (i in 1:ncol(x)) {
    out <- out + add[i + 1] * outer(x[, i], x[, i], "!=")
  }
  out <- as.dist(out)
  attr(out, "method") <- "taxa2dist"
  attr(out, "steps") <- add
  if (missing(labels)) {
    attr(out, "Labels") <- rownames(x)
  }
  else {
    if (length(labels) != nrow(x))
      warning("Labels are wrong: needed ", nrow(x), " got ",
              length(labels))
    attr(out, "Labels") <- as.character(labels)
  }
  if (!check && any(out <= 0))
    warning("you used 'check=FALSE' and some distances are zero -- was this intended?")
  out
}

#' List of available NCBI taxonomy ranks
#' @noRd
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}
mainTaxonomyRank <- function() {
  return(
    c(
      "isolate","strain","pathogroup","serotype","serogroup",
      "forma","formaspecialis","varietas",
      "genotype","morph","subvariety","biotype",
      "subspecies","species","speciessubgroup",
      "speciesgroup","series","section","subgenus","genus",
      "subtribe","tribe", "subfamily","family","superfamily",
      "parvorder","infraorder","suborder","order","superorder",
      "subcohort","cohort","infraclass","subclass","class","superclass",
      "subphylum","phylum","superphylum",
      "subkingdom","kingdom","superkingdom"
    )
  )
}

#' Get full taxonomy ranks and IDs
#' @noRd
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}
get_rank <- function (x) {
  rank_df <- x[, 'rank']
  names(rank_df) <- x[, 'rank']

  id_df <- x[, 'id']
  joined_df <- cbind(
    data.frame(rank_df, stringsAsFactors = FALSE),
    data.frame(id_df, stringsAsFactors = FALSE)
  )
  joined_df$rank_df[!(joined_df$rank_df %in% mainTaxonomyRank())] <- 'no rank'
  joined_df <- within(
    joined_df,
    rank_df[rank_df=='no rank'] <- paste0("norank_", id_df[rank_df=='no rank'])
  )
  df <- data.frame(
    t(data.frame(rev(joined_df$rank_df))), stringsAsFactors = FALSE
  )
  outDf <- data.frame(tip = x[nrow(x), "name"], df, stringsAsFactors = FALSE)
  return(outDf)
}

#' Get taxonomy names and IDs for all ranks
#' @noRd
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}
get_name <- function (x) {
  rank_df <- x[, 'rank']
  names(rank_df) <- x[, 'rank']

  nameDf <- x[, 'name']
  id_df <- x[, 'id']

  joined_df <- cbind(
    data.frame(rank_df,stringsAsFactors=FALSE),
    data.frame(nameDf,stringsAsFactors=FALSE)
  )
  joined_df$rank_df[!(joined_df$rank_df %in% mainTaxonomyRank())] <- 'no rank'
  joined_df <- within(
    joined_df,
    rank_df[rank_df=='no rank'] <- paste0("norank_",id_df[rank_df=='no rank'])
  )
  joined_df$name <- paste0(joined_df$nameDf, "##", joined_df$rank_df)

  df <- data.frame(t(data.frame(rev(joined_df$name))), stringsAsFactors = FALSE)
  outDf <- data.frame(tip = x[nrow(x), "name"], df, stringsAsFactors = FALSE)
  return(outDf)
}

#' Indexing all available ranks (including norank)
#' @noRd
#' @param rankList dataframe, whose each row is a rank list of a taxon
#' @return A dataframe containing a list of all possible ranks and their indexed
#' values.
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}
rank_indexing <- function (rankList) {
  ### get all available ranks from input rankList
  uList <- unlist(rankList[seq(2, length(rankList))])
  # get final list of available ranks (remove NA items)
  allInputRank <- as.character(unique(uList))
  allInputRank <- allInputRank[!is.na(allInputRank)]

  ### initial index for main ranks
  mainRank <- mainTaxonomyRank()
  rank2index <- new.env(hash = TRUE)
  getHash <- Vectorize(get, vectorize.args = "x")
  assignHash <- Vectorize(assign, vectorize.args = c("x", "value"))
  for (i in seq_len(length(mainRank))) rank2index[[mainRank[i]]] <- i

  ### the magic happens here
  for (k in seq_len(nrow(rankList))) {
    ## get rank list for current taxon containing only ranks in allInputRank
    subList <- rankList[k,][!is.na(rankList[k,])]
    filter <- vapply(
      subList, function(x) x %in% allInputRank, FUN.VALUE = logical(1))
    subList <- subList[filter]
    
    ## indexing
    tmpEnv <- new.env(hash = TRUE)
    flag <- 0
    for (i in seq_len(length(subList))) {
      iRank <- subList[i]
      if (is.null(rank2index[[iRank]])) {
        for (j in seq_len(length(subList))) {
          if (j < i) {
            if (!is.null(tmpEnv[[subList[i - j]]])) {
              tmpEnv[[iRank]] <- tmpEnv[[subList[i - j]]] + 1
              break
            }
          } else j = j - 1
        }
      } else {
        # for old rank
        if (i > 1) {
          if (flag == 0) {
            if (!(iRank %in% ls(rank2index))) stop(paste(iRank,"not found!"))
            currentIndex <- rank2index[[iRank]]
          } else {
            if (!(iRank %in% ls(tmpEnv))) {
              tmpEnv[[iRank]] <- tmpEnv[[subList[i-1]]]
            }
            currentIndex <- tmpEnv[[iRank]]
          }
          
          if (currentIndex <= tmpEnv[[subList[i-1]]]) {
            if (flag == 0) {
              tmpEnv[[iRank]] <- tmpEnv[[subList[i-1]]] + 1
              # list of current ranks that whose index should be increased
              candidateList <- unlist(
                mget(
                  ls(rank2index)[!(ls(rank2index) %in% ls(tmpEnv))], rank2index
                )
              )
              candidateList <- candidateList[order(unlist(candidateList))]
              for (cl in seq_len(length(candidateList))) {
                r <- names(candidateList)[cl]
                fromIndex <- rank2index[[iRank]]
                if(subList[i-1] %in% ls(rank2index)) {
                  fromIndex <- rank2index[[subList[i-1]]]
                }
                
                if (rank2index[[r]] > fromIndex) {
                  tmpEnv[[r]] <-
                    rank2index[[r]] + (tmpEnv[[iRank]] - rank2index[[iRank]])
                  flag <- 1
                }
              }
            } else {
              step <- tmpEnv[[subList[i-1]]] - rank2index[[iRank]] + 1
              tmpEnv[[iRank]] <- tmpEnv[[subList[i-1]]] + 1
              for (tmpRank in ls(tmpEnv)) {
                if (tmpEnv[[tmpRank]] >= tmpEnv[[subList[i-1]]]){
                  if (!(tmpRank == iRank) && !(tmpRank == subList[i-1])){
                    tmpEnv[[tmpRank]] <- tmpEnv[[tmpRank]] + step
                  }
                }
              }
            }
            assignHash(ls(tmpEnv), getHash(ls(tmpEnv), tmpEnv), rank2index)
          } else {
            if (is.null(tmpEnv[[iRank]])) {
              tmpEnv[[iRank]] <- rank2index[[iRank]]
            }
          }
        } else {
          tmpEnv[[iRank]] <- rank2index[[iRank]]
        }
      }
    }
    assignHash(ls(tmpEnv), getHash(ls(tmpEnv), tmpEnv), rank2index)
  }
  
  # convert env into dataframe and return
  index2RankList <- lapply(
    seq_len(length(allInputRank)), function (x) {
      data.frame(
        index = rank2index[[allInputRank[x]]],
        rank = allInputRank[x], stringsAsFactors = FALSE
      )
    }
  )
  index2RankDf <- do.call(rbind, index2RankList)
  index2RankDf <- index2RankDf[with(index2RankDf, order(index2RankDf$index)),]
  return(index2RankDf)
}

#' Align NCBI taxonomy IDs of list of taxa into a sorted rank list
#' @noRd
#' @param nameList a dataframe whose each row is a rank+ID list of a taxon
#' @param rankList a dataframe whose each row is a rank list of a taxon
#' @param remove_shared If `TRUE`, remove any taxa that are coarser ranks
#'   present in other taxa, such as both a genus and a species in that genus in
#'   the same tree.
#' @return An aligned taxonomy dataframe which contains all the available
#'   taxonomy ranks from the id list and rank list
#' @author Vinh Tran {tran@bio.uni-frankfurt.de}
taxonomy_table_creator <- function (nameList, rankList, remove_shared = FALSE) {
  colnames(nameList)[1] <- "tip"
  # remove duplicated taxa (e.g. taxa with higher levels, that already belong 
  # to the taxonomy string of other taxa)
  if (remove_shared) {
    duplicatedTaxa <- lapply(
      nameList$X1,
      function (x) {
        matchs <-data.frame(which(nameList[,-1] == x, arr.ind=TRUE))
        checkHigherRank <- lapply(
          matchs$row,
          function(y) {
            if (nameList[y,]$X1 != x) return(1)
          }
        )
        if (length(unlist(checkHigherRank)) > 0) 
          return(strsplit(x, "#", fixed = TRUE)[[1]][1])
      }
    )
    if (length(unlist(duplicatedTaxa)) > 0) {
      msg <- paste("WARNING:", length(unlist(duplicatedTaxa)))
      if (length(unlist(duplicatedTaxa)) == 1)
        msg <- paste(msg, "duplicated taxon has been ignored!")
      else
        msg <- paste(msg, "duplicated taxa have been ignored!")
      if (length(unlist(duplicatedTaxa)) < 5) {
        msg <- paste(msg, "Including: ")
        msg <- c(msg, paste(unlist(duplicatedTaxa), collapse = "; "))
      }   
      message(msg)
    }
    nameList <- nameList[!(nameList$tip %in% unlist(duplicatedTaxa)),]
    rankList <- rankList[!(rankList$tip %in% unlist(duplicatedTaxa)),]
  }
  # get indexed rank list
  index2RankDf <- rank_indexing(rankList)
  # get ordered rank list
  orderedRank <- factor(index2RankDf$rank, levels = index2RankDf$rank)
  # create a dataframe containing ordered ranks
  full_rank_name_df <- data.frame(
    "rank"= matrix(unlist(orderedRank), nrow = length(orderedRank), byrow=TRUE),
    stringsAsFactors = FALSE
  )
  full_rank_name_df$index <- as.numeric(rownames(full_rank_name_df))
  
  fullRankIDdf <- data.frame(
    rank = matrix(
      unlist(orderedRank), nrow = length(orderedRank), byrow = TRUE
    ), stringsAsFactors = FALSE)
  fullRankIDdf$index <- as.numeric(rownames(fullRankIDdf))

  for (i in 1:nrow(nameList)) {
    ### get list of all IDs for this taxon
    taxonDf <- data.frame(nameList[i,])
    taxonName <- unlist(
      strsplit(as.character(nameList[i,]$tip), "##", fixed = TRUE)
    )

    ### convert into long format
    # mTaxonDf <- suppressWarnings(melt(taxonDf,id = "tip"))
    mTaxonDf <- data.table::setDF(
      suppressWarnings(
        data.table::melt(data.table::as.data.table(taxonDf), id = "tip")
      )
    )

    ### get rank names and corresponding IDs
    splitCol <- data.frame(
      do.call(
        'rbind', strsplit(as.character(mTaxonDf$value), "##", fixed=TRUE)
      )
    )
    mTaxonDf <- cbind(mTaxonDf,splitCol)

    ### remove NA cases
    mTaxonDf <- mTaxonDf[complete.cases(mTaxonDf),]

    ### subselect mTaxonDf to keep only 2 column rank id and rank name
    mTaxonDf <- mTaxonDf[,c("X1","X2")]
    if(mTaxonDf$X2[1] != index2RankDf$rank[1] &&
       !index2RankDf$rank[1] %in% mTaxonDf$X2){
      mTaxonDf <- rbind(
        data.frame("X1"=mTaxonDf$X1[1], "X2"=index2RankDf$rank[1]), mTaxonDf
      )
    }

    ### rename columns
    colnames(mTaxonDf) <- c(taxonName[1],"rank")

    ### merge with index2RankDf (Df contains all available ranks of input data)
    full_rank_name_df <- merge(
      full_rank_name_df,mTaxonDf, by=c("rank"), all.x = TRUE
    )

    ### reorder ranks
    full_rank_name_df <- full_rank_name_df[order(full_rank_name_df$index),]

    ### replace NA id by id of previous rank
    full_rank_name_df <- zoo::na.locf(full_rank_name_df)
  }

  ### remove index column
  full_rank_name_df <- subset(full_rank_name_df, select = -c(index))

  ### transpose into wide format
  t_full_rank_name_df <- transpose(full_rank_name_df)

  ### set first row to column names
  colnames(t_full_rank_name_df) = as.character(unlist(t_full_rank_name_df[1,]))
  t_full_rank_name_df <- t_full_rank_name_df[-1,]

  ### replace NA values in the dataframe t_full_rank_name_df
  if(nrow(t_full_rank_name_df[is.na(t_full_rank_name_df),]) > 0){
    t_full_rank_name_dfTMP <- 
      t_full_rank_name_df[complete.cases(t_full_rank_name_df),]
    t_full_rank_name_dfEdit <- t_full_rank_name_df[is.na(t_full_rank_name_df),]

    for(i in 1:nrow(t_full_rank_name_dfEdit)){
      for(j in 1:(ncol(t_full_rank_name_df)-1)){
        if(is.na(t_full_rank_name_dfEdit[i,j])){
          t_full_rank_name_dfEdit[i,j] <- t_full_rank_name_dfEdit[i,j+1]
        }
      }
      if(is.na(t_full_rank_name_dfEdit[i,ncol(t_full_rank_name_df)])){
        t_full_rank_name_dfEdit[i,ncol(t_full_rank_name_df)] <-
        t_full_rank_name_dfEdit[i,ncol(t_full_rank_name_df)-1]
      }
    }
    t_full_rank_name_df <- rbind(t_full_rank_name_dfEdit,t_full_rank_name_dfTMP)
  }

  ### add fullName column into t_full_rank_name_df
  fullName <- colnames(full_rank_name_df)[-c(1)]
  full_rank_name_df <- cbind(fullName,t_full_rank_name_df)

  ### return
  return(full_rank_name_df)
}
