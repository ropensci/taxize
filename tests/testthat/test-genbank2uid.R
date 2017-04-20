context("genbank2uid")

test_that("genbank2uid - with accession numbers", {
  skip_on_cran()

  aa <- genbank2uid(id = 'AJ748748')
  bb <- genbank2uid(c('X78312','KM495596'))

  expect_is(aa, "uid")
  expect_is(unclass(aa), "character")
  expect_is(attr(aa, "match"), "character")
  expect_false(attr(aa, "multiple_matches"))
  expect_false(attr(aa, "pattern_match"))
  expect_is(attr(aa, "uri"), "character")
  expect_match(attr(aa, "uri"), "http")

  expect_is(bb, "uid")
  expect_is(unclass(bb), "character")
  expect_is(attr(bb, "match"), "character")
  expect_equal(attr(bb, "multiple_matches"), c(FALSE, FALSE))
  expect_equal(attr(bb, "pattern_match"), c(FALSE, FALSE))
  expect_is(attr(bb, "uri"), "character")
  expect_match(attr(bb, "uri"), "http")
})

test_that("genbank2uid - with gi numbers", {
  skip_on_cran()

  aa <- genbank2uid(id = 62689767)
  bb <- genbank2uid(c(62689767,156446673))

  expect_is(aa, "uid")
  expect_is(unclass(aa), "character")
  expect_is(attr(aa, "match"), "character")
  expect_false(attr(aa, "multiple_matches"))
  expect_false(attr(aa, "pattern_match"))
  expect_is(attr(aa, "uri"), "character")
  expect_match(attr(aa, "uri"), "http")

  expect_is(bb, "uid")
  expect_is(unclass(bb), "character")
  expect_is(attr(bb, "match"), "character")
  expect_equal(attr(bb, "multiple_matches"), c(FALSE, FALSE))
  expect_equal(attr(bb, "pattern_match"), c(FALSE, FALSE))
  expect_is(attr(bb, "uri"), "character")
  expect_match(attr(bb, "uri"), "http")
})

test_that("genbank2uid - where ID has more than one taxon associated", {
  skip_on_cran()

  aa <- genbank2uid(id = "AM420293")

  expect_is(aa, "list")
  expect_is(aa[[1]], "uid")
  expect_is(aa[[2]], "uid")
  expect_is(aa[[3]], "uid")
  expect_match(attr(aa[[1]], "name"), "Saccharopolyspora")
  expect_match(attr(aa[[2]], "name"), "Saccharopolyspora")
  expect_match(attr(aa[[3]], "name"), "Saccharopolyspora")
})

test_that("genbank2uid - fails well", {
  skip_on_cran()

  # one Id not found
  expect_warning(genbank2uid(id = "gwa2_scaffold_1731_16S_1"),
                 "An error occurred looking up taxon ID\\(s\\)\\.")
  aa <- suppressWarnings(genbank2uid(id = "gwa2_scaffold_1731_16S_1"))
  expect_is(aa, "uid")
  expect_true(is.na(unclass(aa)))

  # many Ids not found
  expect_warning(genbank2uid(id = c("gwa2_scaffold_1731_16S_1", "asdfadfs")),
                 "An error occurred looking up taxon ID\\(s\\)\\.")
  expect_warning(genbank2uid(id = c("gwa2_scaffold_1731_16S_1", "asdfadfs")),
                 "set the 'batch_size' option to 1")
  aa <- suppressWarnings(genbank2uid(id = c("gwa2_scaffold_1731_16S_1",
                                            "asdfadfs")))
  expect_is(aa, "uid")
  expect_true(all(is.na(unclass(aa))))

  # id not given
  expect_error(genbank2uid(), "argument \"id\" is missing")

  # id not given
  expect_error(genbank2uid("Asdfdf", batch_size = "Asdf"),
               "batch_size must be of class integer, numeric")
})
