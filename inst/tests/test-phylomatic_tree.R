# tests for phylomatic_tree fxn in taxize
context("phylomatic_tree")

dat_ <- laply(list("36616", "19322", "183327"), itis_phymat_format, format='rsubmit')
tree <- phylomatic_tree(dat_, 'GET', 'new', 'TRUE')

test_that("phylomatic_tree returns the correct value", {
	expect_that(length(tree$tip.label), equals(3))
})

test_that("phylomatic_tree returns the correct class", {
	expect_that(length(dat_), equals(3))
	expect_that(tree, is_a("phylo"))
})
