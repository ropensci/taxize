test_that("warn_db", {
  expect_null(warn_db(list(), "foo"))
  expect_null(warn_db(NULL, "foo"))
  expect_null(warn_db(list(a = 5), "foo"))
  expect_null(warn_db(list(db = NULL), "foo"))
  expect_null(warn_db(list(db = "foo"), "foo"))
  expect_warning(warn_db(list(db = "bar"), "foo"),
    "'db' value 'bar' ignored; does not match dispatched method 'foo'")
})
