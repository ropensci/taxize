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
test_that("class2tree returns the correct value and class", {
  vcr::use_cassette("class2tree_classification_call", {
    out <- classification(spnames, db = "ncbi", verbose = FALSE)
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
  expect_equal(
    anyDuplicated(gsub("\\.\\d+$", "", names(tr$classification))), 0)
})

test_that("class2tree will abort when input contains duplicate taxa", {
  vcr::use_cassette("class2tree_classification_dup_call", {
    out <- classification(dupnames, db = "ncbi", verbose = FALSE)
  })
  expect_error(class2tree(out),
    "Input list of classifications contains duplicates")
})
