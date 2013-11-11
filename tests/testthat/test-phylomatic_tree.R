context("phylomatic_tree")

taxa <- c("Collomia grandiflora", "Lilium lankongense", "Helianthus annuus")
tree <- phylomatic_tree(taxa=taxa, get = 'POST', informat='newick', 
    method = "phylomatic", storedtree = "smith2011", 
    outformat = "newick", clean = "true")

test_that("phylomatic_tree returns the correct value", {
	expect_that(tree$tip.label[1], equals("Collomia_grandiflora"))
	expect_that(tree$Nnode, equals(2))
})

test_that("phylomatic_tree returns the correct class", {
	expect_that(tree, is_a("phylo"))
	expect_that(plot(tree), is_a('list'))
})

test_that("phylomatic_tree gets the right dimensions", {
  expect_that(length(tree$tip.label), equals(3))
})