context("itis_downstream")

test_that("itis_downstream returns the correct value", {
  data(rank_ref, package = "taxize")
  
  vcr::use_cassette("itis_downstream", {
    dat_ <- itis_downstream(tsns=183264, "Species", verbose=FALSE)
  })

  expect_match(as.character(dat_[1,"rankname"]), "species")
	expect_that(dat_, is_a("data.frame"))
  expect_that(dim(dat_)[2], equals(6))
})
