context("genbank2uid")

test_that("genbank2uid - with accession numbers", {
  vcr::use_cassette("genbank2uid", {
    aa <- genbank2uid(id = 'AJ748748')
    bb <- genbank2uid(c('X78312','KM495596'))
  })

  expect_is(aa, "list")
  expect_is(aa[[1]], "uid")
  expect_is(unclass(aa[[1]]), "character")
  expect_is(attr(aa[[1]], "match"), "character")
  expect_false(attr(aa[[1]], "multiple_matches"))
  expect_false(attr(aa[[1]], "pattern_match"))
  expect_is(attr(aa[[1]], "uri"), "character")
  expect_match(attr(aa[[1]], "uri"), "http")

  expect_is(bb, "list")
  expect_is(bb[[1]], "uid")
  expect_is(unclass(bb[[1]]), "character")
  expect_is(attr(bb[[1]], "match"), "character")
  expect_equal(vapply(bb, attr, TRUE, which = "multiple_matches"), c(FALSE, FALSE))
  expect_equal(vapply(bb, attr, TRUE, which = "pattern_match"), c(FALSE, FALSE))
  expect_is(attr(bb[[1]], "uri"), "character")
  expect_match(attr(bb[[1]], "uri"), "http")
})

test_that("genbank2uid - with gi numbers", {
  vcr::use_cassette("genbank2uid_gi_numbers", {
    aa <- genbank2uid(id = 62689767)
    bb <- genbank2uid(c(62689767,156446673))
  })

  expect_is(aa, "list")
  expect_is(aa[[1]], "uid")
  expect_is(unclass(aa[[1]]), "character")
  expect_is(attr(aa[[1]], "match"), "character")
  expect_false(attr(aa[[1]], "multiple_matches"))
  expect_false(attr(aa[[1]], "pattern_match"))
  expect_is(attr(aa[[1]], "uri"), "character")
  expect_match(attr(aa[[1]], "uri"), "http")

  expect_is(bb, "list")
  expect_is(bb[[1]], "uid")
  expect_is(unclass(bb[[1]]), "character")
  expect_is(attr(bb[[2]], "match"), "character")
  expect_equal(vapply(bb, attr, TRUE, which = "multiple_matches"), c(FALSE, FALSE))
  expect_equal(vapply(bb, attr, TRUE, which = "pattern_match"), c(FALSE, FALSE))
  expect_is(attr(bb[[1]], "uri"), "character")
  expect_match(attr(bb[[1]], "uri"), "http")
})

### THIS ONLY RETURNS 1 RESULT NOW
# test_that("genbank2uid - where ID has more than one taxon associated", {
#   skip_on_cran()
#
#   aa <- genbank2uid(id = "AM420293")
#
#   expect_is(aa, "list")
#   expect_is(aa[[1]], "uid")
#   expect_is(aa[[2]], "uid")
#   expect_is(aa[[3]], "uid")
#   expect_match(attr(aa[[1]], "name"), "Saccharopolyspora")
#   expect_match(attr(aa[[2]], "name"), "Saccharopolyspora")
#   expect_match(attr(aa[[3]], "name"), "Saccharopolyspora")
# })

test_that("genbank2uid - fails well", {
  vcr::use_cassette("genbank2uid_fails", {
    # one Id not found
    expect_warning(genbank2uid(id = "gwa2_scaffold_1731_16S_1"),
                   "The following 1 of 1 queries could not be found")
    aa <- suppressWarnings(genbank2uid(id = "gwa2_scaffold_1731_16S_1"))
    expect_is(aa[[1]], "uid")
    expect_true(is.na(unclass(aa[[1]])))

    # many Ids not found
    expect_warning(genbank2uid(id = c("gwa2_scaffold_1731_16S_1", "asdfadfs")),
                   "The following 2 of 2 queries could not be found")
    expect_warning(genbank2uid(id = c("gwa2_scaffold_1731_16S_1", "asdfadfs")),
                   "The following 2 of 2 queries could not be found")
  })

  skip_on_cran()
  aa <- suppressWarnings(genbank2uid(id = c("gwa2_scaffold_1731_16S_1",
                                            "asdfadfs")))
  expect_is(aa[[1]], "uid")
  expect_true(all(is.na(unclass(aa[[1]]))))

  # id not given
  expect_error(genbank2uid(), "argument \"id\" is missing")

  # id not given
  expect_error(genbank2uid("Asdfdf", batch_size = "Asdf"),
               "batch_size must be of class integer, numeric")
})
