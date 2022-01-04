context("classification.gbif")

test_that("classification works for GBIF data", {
  skip_on_cran()
  vcr::use_cassette("classification_gbif", {
    clas_gbif <- classification(c("Quercus douglasii", "aaa vva"),
      db = "gbif", rows = 1, messages = FALSE)
    names(clas_gbif) <- NULL
  })

  expect_null(clas_gbif[[2]])
  expect_is(clas_gbif, "classification")
  expect_is(clas_gbif[[1]], "data.frame")
  expect_equal(length(clas_gbif), 2)
  # rank is lowercase
  expect_false(all(grepl("[A-Z]", clas_gbif[[1]]$rank)))

  vcr::use_cassette("classification_gbif_get_fxn", {
    gbifs <- get_gbif(c("Quercus douglasii", "aaa vva"), rows = 1,
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
    df[df$name == "Poa annua", "id", drop = T],
    2704179
  )
})

test_that("searches for ranks below species work", {
  skip_on_cran()
  
  vcr::use_cassette("classification_gbif_ranks_below_species_subsp", {
    subsp <- classification(6162875, db = "gbif")
  })
  expect_is(subsp, "classification")
  expect_equal(attr(subsp, "db"), "gbif")
  rank_seq <- vapply(subsp[[1]]$rank, which_rank, 1)
  expect_true(identical(rank_seq, sort(rank_seq)))
  # subspecies rank is expected value
  df <- subsp[[1]]
  expect_equal(
    df[df$name == "Boa constrictor nebulosa", "id", drop = T],
    6162875
  )

  vcr::use_cassette("classification_gbif_ranks_below_species_var", {
    var <- classification(8286319, db = "gbif")
  })
  expect_is(var, "classification")
  expect_equal(attr(var, "db"), "gbif")
  rank_seq <- vapply(var[[1]]$rank, which_rank, 1)
  expect_true(identical(rank_seq, sort(rank_seq)))
  # variety rank is expected value
  df <- var[[1]]
  expect_equal(
    df[df$name == "Poa annua annua", "id", drop = T],
    8286319
  )
})
