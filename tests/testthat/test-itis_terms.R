context("itis_terms")

# test_that("itis_terms returns the correct class", {
#   skip_on_cran()
#   vcr::use_cassette("itis_terms", {
#     one <- itis_terms(query='bear', verbose=FALSE)
#     two <- itis_terms(query='tarweed', "common", verbose=FALSE)
#     three <- itis_terms(query='Poa annua', "scientific", verbose=FALSE)
#   })
# 
#   expect_is(one, "data.frame")
#   expect_is(two, "data.frame")
#   expect_is(three, "data.frame")
# })
