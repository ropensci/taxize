context("col_search_paginate_paginate")

test_that("col_search_paginate works", {
  skip_on_cran()
  
  temp <- col_search_paginate(name = "Apis")
  # two <- col_search_paginate(name = c("Apis", "Puma concolor"))
  # byid <- col_search_paginate(id = "36c623ad9e3da39c2e978fa3576ad415")

  expect_equal(names(temp), "Apis")
  expect_equal(temp[[1]]$name[1], "Apis")

  expect_equal(names(temp), "Apis")
  expect_equal(temp[[1]]$name[1], "Apis")

  expect_true(is.null(dim(temp)))
  expect_equal(NCOL(temp[[1]]), 10)

  # expect_that(length(two), equals(2))
  # expect_equal(NCOL(two[[1]]), 10)
  # expect_equal(NCOL(do.call(rbind, lapply(two, data.frame))), 10)
  # expect_is(byid, "list")
  # expect_is(byid[[1]], "data.frame")

  # expect_that(temp, is_a("list"))
  # expect_that(temp[[1]], is_a("data.frame"))

  # expect_that(two, is_a("list"))
  # expect_that(two[[1]], is_a("data.frame"))
})

# some of these do HTTP requests
test_that("col_search_paginate is robust to user error", {
  skip_on_cran()
  expect_is(sw(col_search_paginate(name = "asdfsdf"))[[1]], "data.frame")
  # expect_is(sw(col_search_paginate(name = ""))[[1]], "data.frame")
  expect_is(col_search_paginate(), "list")
  expect_equal(length(col_search_paginate()), 0)
})
