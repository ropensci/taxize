context("classification.pow")
test_that("classification works for POW data", {
  vcr::use_cassette("classification_pow", {
    clas_pow <- classification(c("Quercus douglasii", "aaa vva"), db = "pow",
                                messages = FALSE)
    names(clas_pow) <- NULL
  })

  expect_that(clas_pow[[2]], equals(NA))
  expect_is(clas_pow, "classification")
  expect_is(clas_pow[[1]], "data.frame")
  expect_equal(length(clas_pow), 2)
  # rank is lowercase
  expect_false(all(grepl("[A-Z]", clas_pow[[1]]$rank)))

  vcr::use_cassette("classification_pow_get_fxn", {
    pows <- get_pow(c("Quercus douglasii", "aaa vva"), messages = FALSE)
    clas_pows <- classification(pows, messages = FALSE)
    names(clas_pows) <- NULL
  })

  expect_identical(clas_pows, clas_pow)
})

test_that("passing in an id works", {
  vcr::use_cassette("classification_pow_passing_id", {
    fromid_pow <- classification("urn:lsid:ipni.org:names:119003-2",
      db = "pow")
  })

  expect_is(fromid_pow, "classification")
  expect_equal(attr(fromid_pow, "db"), "pow")
})

test_that("rbind and cbind work correctly", {
  vcr::use_cassette("classification_pow_cbind_rbind", {
    out <- get_ids( 
      c("Brillantaisia vogeliana", "Aphelandra aurantiaca"),
      db = "pow", messages = FALSE)
    cl <- classification(out)
  }, preserve_exact_body_bytes = TRUE)

  # rbind
  clr <- rbind(cl)
  expect_is(clr, "data.frame")
  expect_named(clr, c("name", "rank", "id", "query", "db"))

  # cbind
  clc <- cbind(cl)
  expect_is(clc, "data.frame")
  expect_gt(length(names(clc)), 5)
})

test_that("works on a variety of names", {
  vcr::use_cassette("classification_pow_more_name_egs", {
    x <- classification("Mimosa weberbaueri", db = "pow", messages = FALSE)
    z <- classification("Cyperus zollingeri", db = "pow", messages = FALSE)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(x, "classification")
  expect_is(z, "classification")
})

test_that("queries with no results fail well", {
  vcr::use_cassette("classification_pow_no_results", {
    aa <- classification(x = "foobar", db = "pow", messages = FALSE)
    bb <- classification(get_pow("foobar", messages = FALSE), messages = FALSE)
  })

  expect_true(is.na(unclass(aa)[[1]]))
  expect_identical(unname(aa), unname(bb))
})
