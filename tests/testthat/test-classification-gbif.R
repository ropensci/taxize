context("classification.gbif")

test_that("classification works for GBIF data", {
  skip_on_cran()
  vcr::use_cassette("classification_gbif", {
    clas_gbif <- classification(c("Quercus douglasii", "aaa vva"),
      db = "gbif", rows = 1, messages = FALSE)
    names(clas_gbif) <- NULL
  })

  expect_that(clas_gbif[[2]], equals(NA))
  expect_is(clas_gbif, "classification")
  expect_is(clas_gbif[[1]], "data.frame")
  expect_equal(length(clas_gbif), 2)
  # rank is lowercase
  expect_false(all(grepl("[A-Z]", clas_gbif[[1]]$rank)))

  vcr::use_cassette("classification_gbif_get_fxn", {
    gbifs <- get_gbifid(c("Quercus douglasii", "aaa vva"), rows = 1,
      messages = FALSE)
    clas_gbifs <- classification(gbifs, messages = FALSE)
    names(clas_gbifs) <- NULL
  })

  expect_identical(clas_gbifs, clas_gbif)
})

test_that("ranks are in the correct order", {
  skip_on_cran()
  vcr::use_cassette("classification_gbif_ranks_correct_order", {
    fromid_gbif <- classification(2704179, db = "gbif")
  })

  expect_is(fromid_gbif, "classification")
  expect_equal(attr(fromid_gbif, "db"), "gbif")
  rank_seq <- vapply(fromid_gbif[[1]]$rank, which_rank, 1)
  expect_true(identical(rank_seq, sort(rank_seq)))
  # species rank is expected value
  df <- fromid_gbif[[1]]
  expect_equal(
    df[df$name == "Poa annua", "id"],
    2704179
  )
})
