context("phylomatic_tree")

data("apg_families", package = "taxize")

taxa <- c("Poa annua", "Phlox diffusa", "Helianthus annuus")
tree <- suppressWarnings(phylomatic_tree(taxa=taxa, get = 'GET', verbose = FALSE))
tree2 <- suppressWarnings(phylomatic_tree(taxa=taxa, get = 'POST', verbose = FALSE))

mynames <- c('Astragalus monspessulanus', 'Tagetes oaxacana', 'Gloxinia donkelaariana', 'Diuris brevissima', 'Schefflera sodiroi', 'Brachylaena rotundata', 'Lapsanastrum takasei', 'Jurinea carduiformis', 'Miconia imitans', 'Goodenia granitica', 'Nymphaea mexicana', 'Sertula coerulea', 'Begonia gulinqingensis', 'Byttneria piresii', 'Dioscorea oblonga', 'Sinningia sellovii', 'Atherolepis wallichii', 'Hieracium lachenalii', 'Euphorbia personata', 'Leptodactylon pungens')
tree3 <- suppressWarnings(phylomatic_tree(taxa=mynames, get = 'POST', verbose = FALSE))

test_that("phylomatic_tree returns the correct value", {
	expect_that(tree$tip.label[1], equals("poa_annua"))
	expect_that(tree$Nnode, equals(2))
	expect_that(tree2$Nnode, equals(2))
	expect_that(tree3$Nnode, equals(16))
})

test_that("phylomatic_tree GET and POST return identical results", {
  expect_identical(tree, tree2)
})

test_that("phylomatic_tree returns the correct class", {
	expect_that(tree, is_a("phylo"))
	expect_that(plot(tree), is_a('list'))
})

test_that("phylomatic_tree gets the right dimensions", {
  expect_that(length(tree$tip.label), equals(3))
  expect_that(length(tree2$tip.label), equals(3))
  expect_that(length(tree3$tip.label), equals(17))
})
