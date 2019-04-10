context("taxon_state/taxon_last")

test_that("taxon_last", {
  # before anything happens, it's NULL
  expect_null(taxon_last())

  # something happens
  invisible(taxon_state$new())

  # now it's not NULL
  expect_is(taxon_last(), "taxon_state")

  # in more real context: get_gbifid call
  spp <- c("Spruceanthus marianus", "Bomarea costaricensis",
    "Rubus mindanaensis", "Columnea mira")
  res <- get_gbifid(spp, messages = FALSE)
  tl <- taxon_last()
  expect_is(tl, "taxon_state")
  expect_equal(tl$count, 4)
  expect_equal(tl$class, "gbifid")
})

test_that("taxon_state works", {
  ts <- taxon_state$new()

  expect_is(ts$add, "function")
  expect_is(ts$get, "function")
  expect_is(ts$remove, "function")
  expect_is(ts$purge, "function")
  expect_is(ts$count, "integer")
  expect_is(ts$exit, "POSIXct")

  expect_equal(ts$count, 0)

  # add something
  res <- list(
    id = 123456,
    att = "found",
    multiple = FALSE,
    direct = FALSE,
    class = "tsn"
  )
  ts$add(query = "Quercus robur", result = res)
  expect_equal(ts$count, 1)

  bb <- ts$get(query = "Quercus robur")
  expect_is(bb, "list")
  expect_is(bb$id, "numeric")
  expect_is(bb$att, "character")
  expect_is(bb$multiple, "logical")
  expect_is(bb$direct, "logical")
  expect_is(bb$class, "character")
  
  # remove something
  ts$remove(query = "Quercus robur")
  expect_equal(ts$count, 0)

  # add many records
  res2 <- list(
    id = 3430834535,
    att = "found",
    multiple = FALSE,
    direct = FALSE,
    class = "gbifid"
  )
  ts$add(query = "Quercus robur", result = res)
  ts$add(query = "Poa annua", result = res2)
  expect_equal(ts$count, 2)

  # purge all records
  ts$purge()
  expect_equal(ts$count, 0)
})

test_that("taxon_state fails well", {
  expect_error(taxon_state$new(apple = 5), "unused argument")
})
