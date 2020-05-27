test_that("rank_ref", {
  data(rank_ref, package = "taxize")

  expect_is(rank_ref, "data.frame")
  expect_named(rank_ref, c("rankid", "ranks"))
  expect_is(rank_ref$rankid, "character")
  expect_is(rank_ref$ranks, "character")
  expect_gt(NROW(rank_ref), 40)
  
  # rank ids are in order
  z <- sort(as.numeric(rank_ref$rankid))
  expect_identical(z, sort(z))

  # rank name stuff
  w <- rank_ref$ranks
  ## some have commas in them
  expect_true(any(grepl(",", w)))
  ## all are lowercase
  expect_identical(w, tolower(w))
})
