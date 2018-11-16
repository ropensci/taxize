# tests for col_children fxn in taxize
context("col_children")

test_that("col_children returns the correct classes and dimensions", {
  skip_on_cran()

  temp4 <- col_children(name="Animalia")
  temp5 <- col_children(name="Plantae")
  temp11 <- col_children(name="Accipiter striatus")
  temp13 <- col_children(id=c(2346405,2344165,2346405), checklist = '2012')

	expect_is(temp4, "list")
  expect_is(temp5, "list")
  expect_is(temp11, "list")
  expect_is(temp4$Animalia, "data.frame")
  expect_is(temp5$Plantae, "data.frame")
  expect_is(temp11$`Accipiter striatus`, "data.frame")
  expect_is(temp13$`2346405`, "data.frame")

	expect_equal(NCOL(temp4[[1]]), 4)
	expect_equal(NCOL(temp5[[1]]), 4)

	expect_equal(length(temp11), 1)
})

test_that("missing/wrong data given returns result", {
  skip_on_cran()

  expect_equal(nrow(col_children(name="")[[1]]), 0)
  expect_equal(nrow(col_children(name="asdfasdfdf")[[1]]), 0)
  expect_equal(unname(sapply(col_children(name="asdfasdfdf")[[1]], class)), 
    c(rep('character', 3), "logical"))
  expect_is(col_children(), "list")
})
