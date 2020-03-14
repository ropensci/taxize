context("class2tree")

spnames <- c('Homo_sapiens','Pan_troglodytes','Macaca_mulatta','Mus_musculus',
             'Rattus_norvegicus','Bos_taurus','Canis_lupus',
             'Ornithorhynchus_anatinus','Xenopus_tropicalis',
             'Takifugu_rubripes','Gallus_gallus','Ciona_intestinalis',
             'Branchiostoma_floridae','Schistosoma_mansoni',
             'Caenorhabditis_elegans','Anopheles_gambiae',
             'Drosophila_melanogaster','Ixodes_scapularis',
             'Ustilago_maydis','Neurospora_crassa','Monodelphis_domestica',
             'Danio_rerio','Nematostella_vectensis','Cryptococcus_neoformans')

dupnames <- c("Mus musculus", "Escherichia coli",
              "Haloferax denitrificans", "Mus musculus")

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

test_that("class2tree will abort when input contains duplicate taxa", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("class2tree_classification_dup_call", {
    out <- classification(dupnames, db = "ncbi", messages = FALSE)
  })
  expect_error(class2tree(out),
    "Input list of classifications contains duplicates")
})
