context("class2tree")

spnames <- c("Klattia flava", "Trollius sibiricus", "Arachis paraguariensis",
             "Tanacetum boreale", "Gentiana yakushimensis", "Sesamum schinzianum",
             "Pilea verrucosa", "Tibouchina striphnocalyx", "Lycium dasystemum",
             "Berkheya echinacea", "Androcymbium villosum",
             "Helianthus annuus", "Madia elegans", "Lupinus albicaulis",
             "Pinus lambertiana", "Haloarcula amylolytica JCM 13557",
             "Halomonas sp. 'Soap Lake #6'")

dupnames <- c("Mus musculus", "Escherichia coli",
              "Haloferax denitrificans", "Mus musculus")

duptaxa <- c("Haliotis", "Haliotis cracherodii", "Haliotis rufescens", 
             "Megabalanus californicus")

test_that("internal functions of class2tree", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("class2tree_internal_fxns", {
    out <- classification(spnames, db = "ncbi", messages = FALSE)
    spnames <- c("Proteus mirabilis","Citrus sinensis","Cyanophora paradoxa")
    out2 <- classification(spnames, db = "ncbi", messages = FALSE)
  })
  
  rankList <- dt2df(lapply(out, get_rank), idcol = FALSE)
  expect_equal(nrow(rankList), 17)
  
  nameList <- dt2df(lapply(out, get_name), idcol = FALSE)
  expect_true(identical(colnames(nameList), colnames(rankList)))
  
  strainIndex <- grep("norank", rankList$X1)
  rankList$X1[strainIndex] <- "strain"
  nameList$X1[strainIndex] <- 
    gsub("norank_[[:digit:]]+", "strain", nameList$X1[strainIndex])
  expect_true(length(grep("norank", rankList$X1)) == 0)
  
  indexedRank <- rank_indexing(rankList)
  expect_true(tail(indexedRank, 1)$rank == "norank_131567")
  
  taxMatrix <- taxonomy_table_creator(nameList, rankList)
  expect_is(taxMatrix, "data.frame")
  expect_true(nrow(taxMatrix) == 17)
  
  # wrong indexing
  rankList <- dt2df(lapply(out2, get_rank), idcol = FALSE)
  rankList[3,] <- c(
    "Cyanophora paradoxa","species","genus","family","superkingdom",
    "norank_131567","class",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  )
  indexedRank <- rank_indexing(rankList)
  expect_true(
    indexedRank$index[indexedRank$rank == "kingdom"] <
      indexedRank$index[indexedRank$rank == "class"]
  )
})

test_that("class2tree returns the correct value and class", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("class2tree_classification_call", {
    out <- classification(spnames, db = "ncbi", messages = FALSE)
  })

  out <- out[!is.na(out)]
  tr <- class2tree(out)

  expect_identical(tr$names, names(out))

  expect_is(plot(tr), "list")
  expect_is(tr, "classtree")
  expect_is(tr$phylo, "phylo")
  expect_is(tr$classification, "data.frame")
  expect_is(tr$distmat, "dist")
  expect_is(tr$names, "character")
  expect_is(tr$phylo$node.label, "character")
  expect_equal(length(tr$phylo$node.label), tr$phylo$Nnode)
  expect_equal(
    anyDuplicated(gsub("\\.\\d+$", "", names(tr$classification))), 0)
})

test_that("class2tree will abort when input contains duplicated taxa", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("class2tree_classification_dup_call", {
    out <- classification(dupnames, db = "ncbi", messages = FALSE)
  })
  expect_error(class2tree(out),
    "Input list of classifications contains duplicates")
})

test_that("class2tree detects duplicated taxa in higher levels", {
    skip_on_cran() # uses secrets
    vcr::use_cassette("class2tree_classification_dup_high_level", {
        out <- classification(duptaxa, db = "ncbi", messages = FALSE)
    })
    tree <- class2tree(out, remove_shared = TRUE)
    expect_true(nrow(tree$classification) < length(duptaxa))
})
