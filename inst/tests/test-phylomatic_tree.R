# tests for phylomatic_tree fxn in taxize
context("phylomatic_tree")

taxa <- c("Poa annua", "Abies procera", "Helianthus annuus")
tree <- phylomatic_tree(taxa=taxa, get = 'POST', informat='newick', method = "phylomatic", 
		storedtree = "R20120829", taxaformat = "slashpath", outformat = "newick", clean = "true")

test_that("phylomatic_tree returns the correct value", {
	expect_that(Ntip(tree), equals(3))
})

test_that("phylomatic_tree returns the correct class", {
	expect_that(tree, is_a("phylo"))
})