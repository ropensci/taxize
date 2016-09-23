context("itis_acceptname")

test_that("itis_acceptname works with accepted tsn", {
  skip_on_cran()

  temp <- itis_acceptname(208527)

	expect_is(temp, "data.frame")
	expect_named(temp, c('submittedtsn', 'acceptedname', 'acceptedtsn', 'author'))
	expect_equal(temp$submittedtsn, 208527)
	expect_true(is.na(temp$acceptedname))
})

test_that("itis_acceptname works with non-accepted tsn", {
  skip_on_cran()

  temp <- itis_acceptname(504239)

  expect_is(temp, "data.frame")
  expect_named(temp, c('submittedtsn', 'acceptedname', 'acceptedtsn', 'author'))
  expect_equal(temp$submittedtsn, 504239)
  expect_that(is.na(temp$acceptedname), is_false())
})

test_that("itis_acceptname fails as expected", {
  expect_error(itis_acceptname(), "\"searchtsn\" is missing")
})
