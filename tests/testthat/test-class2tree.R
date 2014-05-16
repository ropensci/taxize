# tests for class2tree fxn in taxize
context("class2tree")

# spnames <- names_list('species', 30)
spnames <- c('Physoplexis comosa','Nidorella zavattarii','Centaurea musimomum','Portulaca striata','Cerastium candidissimum','Faramea bicolor','Garrya salicifolia','Clerodendrum curranii','Campanula khorasanica','Micropholis gardneriana','Manilkara excelsa','Fimbristylis alboviridis','Sideritis montana','Polianthes longiflora')
spnames2 <- c('Elachanthemum intricatum','Trichocoronis wrightii','Solanum valdiviense','Ilex perado','Aristida lazaridis','Verticordia blepharophylla','Coleocephalocereus purpureus','Catasetum vinaceum','Narcissus bujei','Erigeron hyperboreus','Ceiba speciosa','Kosteletzkya grantii','Pavonia alnifolia','Saxifraga petraea','Tephrosia decaryana','Astragalus buchtormensis')
out <- classification(spnames, db='ncbi', verbose=FALSE)
out2 <- classification(spnames2, db='ncbi', verbose=FALSE)
out <- out[!is.na(out)]
out2 <- out2[!is.na(out2)]
# length(out)
# rm(tr)
tr <- class2tree(out)
tr2 <- class2tree(out2)
# (tr <- class2tree(out, check = FALSE))

test_that("class2tree returns the correct value", {
  expect_identical(tr$names, names(out))
  expect_identical(tr2$names, names(out2))  
})

test_that("class2tree returns the correct class", {
  expect_is(plot(tr), "list")
  expect_is(plot(tr2), "list")
  expect_is(tr, "classtree")
  expect_is(tr$phylo, "phylo")
  expect_is(tr$classification, "data.frame")
  expect_is(tr$distmat, "dist")
  expect_is(tr$names, "character")
})