context("get_wormsid")

test_that("get_wormsid returns the correct value", {
  skip_on_cran()

  expect_true(is.na(sw(get_wormsid(c('Gadus morhua', "howdy"), verbose=FALSE))[2]))
})

test_that("get_wormsid returns the correct class", {
  skip_on_cran()

  expect_is(
    sw(get_wormsid(c("Platanista gangetica", "Lichenopora neapolitana"),
                   verbose=FALSE)),
            "wormsid")
})

test_that("get_wormsid accepts ask-argument", {
  skip_on_cran()

  expect_is(sw(get_wormsid('Platanista gangetica', ask = FALSE, verbose=FALSE)),
              "wormsid")
  expect_true(is.na(get_wormsid('asdasf', ask = FALSE, verbose=FALSE)))
})

test_that("get_wormsid query modifiers work", {
  skip_on_cran()

  ### w/ modifiers to the name
  mod1 <- sw(get_wormsid('Platanista gangetica', verbose=FALSE))
  mod2 <- sw(get_wormsid('asiatic clam', "common", verbose=FALSE))

  expect_is(mod1, "wormsid")
  expect_is(mod2, "wormsid")
  expect_equal(mod1[1], "254967")
  expect_equal(mod2[1], "181580")
})

test_that("get_wormsid fails well", {
  skip_on_cran()

  expect_true(is.na(get_wormsid("asdfadsf", verbose = FALSE)))

  expect_error(get_wormsid(), "argument \"query\" is missing")
  expect_error(get_wormsid("clam", 5),
               "searchtype must be of class character")
  expect_error(get_wormsid("clam", "stuff", verbose = FALSE),
               "'searchtype' must be one of")
  expect_error(get_wormsid("clam", accepted = 4),
               "accepted must be of class logical")
  expect_error(get_wormsid("clam", ask = 4),
               "ask must be of class logical")
})
