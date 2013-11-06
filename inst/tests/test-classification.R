# tests for classification fxn in taxize
context("classification")

uids <- get_uid(c("Chironomus riparius", "aaa vva"), verbose=FALSE)
tsns <- get_tsn(c("Chironomus riparius", "aaa vva"), verbose=FALSE)
# eolids <- get_tsn(c("Chironomus riparius", "aaa vva"), verbose=FALSE)
colids <- get_colid(c("Chironomus riparius", "aaa vva"), verbose=FALSE)
tpsids <- get_tpsid(c("Helianthus excubitor", "aaa vva"), verbose=FALSE)
clas_uids <- classification(uids, verbose=FALSE)
clas_tsns <- classification(tsns, verbose=FALSE)
# clas_eolids <- classification(eolids, verbose=FALSE)
clas_colids <- classification(colids)
clas_tpids <- classification(tpsids)

clas_ncbi <- classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi', 
                            verbose=FALSE)
names(clas_ncbi) <- NULL

clas_itis <- classification(c("Chironomus riparius", "aaa vva"), db = 'itis', 
                            verbose=FALSE)
names(clas_itis) <- NULL

# clas_eol <- classification(c("Pinus amabilis", "aaa vva"), db = 'eol')
# names(clas_eol) <- NULL

clas_col <- classification(c("Chironomus riparius", "aaa vva"), db = 'col')
names(clas_col) <- NULL

clas_tp <- classification(c("Helianthus excubitor", "aaa vva"), db = 'tropicos')
names(clas_tp) <- NULL

test_that("classification returns the correct value", {
	expect_that(clas_ncbi[[2]], equals(NA))
	expect_that(clas_itis[[2]], equals(NA))
# 	expect_that(clas_eol[[2]], equals(NA))
	expect_that(clas_col[[2]], equals(NA))
	expect_that(clas_tp[[2]], equals(NA))
})

test_that("classification returns the correct class", {
	expect_that(clas_ncbi, is_a("list"))
	expect_that(clas_ncbi[[1]], is_a("data.frame"))
	expect_that(length(clas_ncbi), equals(2))
	
  expect_that(clas_itis, is_a("list"))
	expect_that(clas_itis[[1]], is_a("data.frame"))
  expect_that(length(clas_itis), equals(2))
  
# 	expect_that(clas_eol, is_a("list"))
# 	expect_that(clas_eol[[1]], is_a("data.frame"))
# 	expect_that(length(clas_eol), equals(2))
  
	expect_that(clas_col, is_a("list"))
	expect_that(clas_col[[1]], is_a("data.frame"))
	expect_that(length(clas_col), equals(2))
  
	expect_that(clas_tp, is_a("list"))
	expect_that(clas_tp[[1]], is_a("data.frame"))
	expect_that(length(clas_tp), equals(2))
})

test_that("check S3-methods for tsn and uid class", {
  expect_identical(clas_uids, clas_ncbi)
  expect_equal(clas_tsns, clas_itis)
#   expect_identical(clas_eolids, clas_ncbi)
  expect_identical(clas_colids, clas_col)
  expect_identical(clas_tpids, clas_tp)
})