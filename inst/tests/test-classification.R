# tests for classification fxn in taxize
context("classification")

uids <- get_uid(c("Chironomus riparius", "aaa vva"), verbose=FALSE)
tsns <- get_tsn(c("Chironomus riparius", "aaa vva"), verbose=FALSE)
clas_uids <- classification(uids, verbose=FALSE)
clas_tsns <- classification(tsns, verbose=FALSE)

clas_ncbi <- classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi', 
                            verbose=FALSE)
names(clas_ncbi) <- NULL
clas_itis <- classification(c("Chironomus riparius", "aaa vva"), db = 'itis', 
                            verbose=FALSE)
names(clas_itis) <- NULL

test_that("classification returns the correct value", {
	expect_that(clas_ncbi[[2]], equals(NA))
	expect_that(clas_itis[[2]], equals(NA))
})

test_that("classification returns the correct class", {
	expect_that(clas_ncbi, is_a("list"))
	expect_that(clas_ncbi[[1]], is_a("data.frame"))
	expect_that(length(clas_ncbi), equals(2))
	expect_that(clas_itis, is_a("list"))
	expect_that(clas_itis[[1]], is_a("data.frame"))
  expect_that(length(clas_itis), equals(2))
})

test_that("check S3-methods for tsn and uid class", {
  expect_identical(clas_uids, clas_ncbi)
  expect_equal(clas_tsns, clas_itis)
})