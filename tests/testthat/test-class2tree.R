# tests for class2tree fxn in taxize
context("class2tree")

spnames <- c('Klattia flava', 'Trollius sibiricus', 'Arachis paraguariensis',
 'Tanacetum boreale', 'Gentiana yakushimensis','Sesamum schinzianum',
 'Pilea verrucosa','Tibouchina striphnocalyx','Lycium dasystemum',
 'Berkheya echinacea','Androcymbium villosum',
 'Helianthus annuus','Madia elegans','Lupinus albicaulis',
 'Pinus lambertiana')

test_that("class2tree returns the correct value and class", {
  skip_on_cran()

  out <- classification(spnames, db = 'ncbi', verbose = FALSE)
  out <- out[!is.na(out)]
  tr <- class2tree(out)

  expect_identical(tr$names, names(out))

  expect_is(plot(tr), "list")
  expect_is(tr, "classtree")
  expect_is(tr$phylo, "phylo")
  expect_is(tr$classification, "data.frame")
  expect_is(tr$distmat, "dist")
  expect_is(tr$names, "character")
})
