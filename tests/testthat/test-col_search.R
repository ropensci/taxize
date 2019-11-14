context("col_search")

test_that("col_search returns the correct values, dimensions, and classes", {
  skip_on_cran()
  vcr::use_cassette("col_search", {
    temp <- col_search(name = "Apis")
    two <- col_search(name = c("Apis","Puma concolor"))
    byid <- col_search(id = "36c623ad9e3da39c2e978fa3576ad415")
  })

	expect_equal(names(temp), "Apis")
	expect_equal(temp[[1]]$name[1], "Apis")

	expect_equal(names(temp), "Apis")
	expect_equal(temp[[1]]$name[1], "Apis")

  expect_true(is.null(dim(temp)))
  expect_equal(NCOL(temp[[1]]), 10)

  expect_that(length(two), equals(2))
  expect_equal(NCOL(two[[1]]), 10)
  expect_equal(NCOL(do.call(rbind, lapply(two, data.frame))), 10)
  expect_is(byid, "list")
  expect_is(byid[[1]], "data.frame")

	expect_is(temp, "list")
	expect_is(temp[[1]], "data.frame")

	expect_is(two, "list")
	expect_is(two[[1]], "data.frame")
})

test_that("2014 year or older returns xmls", {
  skip_on_cran()
  vcr::use_cassette("col_search_older_checklist_xml", {
    aa <- col_search(name = "Apis", checklist = 2013)
  })

  expect_equal(names(aa), "Apis")
  expect_is(aa[[1]], "xml_document")
})

test_that("col_search is robust to user error", {
  skip_on_cran()

  Sys.sleep(1)
  expect_is(sw(col_search(name = "asdfsdf"))[[1]], "data.frame")
  Sys.sleep(1)
  expect_is(sw(col_search(name = ""))[[1]], "data.frame")
  Sys.sleep(0.5)
  expect_is(col_search(), "list")
  expect_equal(length(col_search()), 0)
})
