context("itis_acceptname")

test_that("itis_acceptname works with accepted tsn", {
  vcr::use_cassette("itis_acceptname", {
    temp <- itis_acceptname(208527)
  })

  expect_is(temp, "data.frame")
	expect_named(temp, c('submittedtsn', 'acceptedname', 'acceptedtsn', 'author'))
	expect_equal(temp$submittedtsn, 208527)
	expect_true(is.na(temp$acceptedname))
})

test_that("itis_acceptname works with non-accepted tsn", {
  vcr::use_cassette("itis_acceptname_non_accepted_tsn", {
    temp <- itis_acceptname(504239)
  })

  expect_is(temp, "data.frame")
  expect_named(temp, c('submittedtsn', 'acceptedname', 'acceptedtsn', 'author'))
  expect_equal(temp$submittedtsn, 504239)
  expect_false(is.na(temp$acceptedname))
})

test_that("itis_acceptname fails as expected", {
  expect_error(itis_acceptname(), "\"searchtsn\" is missing")
})
