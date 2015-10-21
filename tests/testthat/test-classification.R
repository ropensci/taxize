# tests for classification fxn in taxize
context("classification")

# is_up <- function(seconds=3, which="itis"){
#   itisfxn <- function(x) tryCatch(itis_ping(config=timeout(x)), error=function(e) e)
#   eolfxn <- function(x) tryCatch(eol_ping(config=timeout(x)), error=function(e) e)
#   switch(which,
#          itis = !is(itisfxn(seconds), "OPERATION_TIMEDOUT"),
#          eol = !is(eolfxn(seconds), "OPERATION_TIMEDOUT")
#   )
# }
#
# is_up()
# is_up(which = "eol")
# is_up(which = "col")
# is_up(which = "ncbi")
#
# skip_if <- function(x){
#   if(!res) skip("API down")
#
#   expect_is(classification(c("Chironomus riparius", "aaa vva"), db = 'itis', verbose=FALSE), "classification")
# }

uids <- get_uid(c("Chironomus riparius", "aaa vva"), verbose=FALSE)
tsns <- get_tsn(c("Chironomus riparius", "aaa vva"), verbose=FALSE)
# eolids <- get_tsn(c("Chironomus riparius", "aaa vva"), verbose=FALSE)
# colids <- get_colid(c("Chironomus riparius", "aaa vva"), verbose=FALSE)
# tpsids <- get_tpsid(sciname=c("Helianthus excubitor", "aaa vva"), verbose=FALSE)
clas_uids <- classification(uids, verbose=FALSE)
names(clas_uids) <- NULL
clas_tsns <- classification(tsns, verbose=FALSE)
names(clas_tsns) <- NULL
# clas_eolids <- classification(eolids, verbose=FALSE)
# clas_colids <- classification(colids)
# clas_tpids <- classification(tpsids, verbose=FALSE)

clas_ncbi <- classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi',
                            verbose=FALSE)
names(clas_ncbi) <- NULL

clas_itis <- classification(c("Chironomus riparius", "aaa vva"), db = 'itis', verbose=FALSE)
names(clas_itis) <- NULL

# clas_eol <- classification(c("Helianthus petiolaris Nutt.", "aaa vva"), db = 'eol')
# names(clas_eol) <- NULL

# clas_col <- suppressMessages(classification(c("Puma concolor", "aaa vva"), db = 'col'))
# names(clas_col) <- NULL
# colids <- get_colid(c("Puma concolor", "aaa vva"), verbose=FALSE)
# clas_colids <- classification(colids)
# names(clas_colids) <- NULL

# clas_tp <- suppressMessages(classification(c("Helianthus excubitor", "aaa vva"), db = 'tropicos'))
# names(clas_tp) <- NULL

test_that("classification returns the correct value", {
	expect_that(clas_ncbi[[2]], equals(NA))
	expect_that(clas_itis[[2]], equals(NA))
# 	expect_that(clas_eol[[2]], equals(NA))
# 	expect_that(clas_col[[2]], equals(NA))
# 	expect_that(clas_tp[[2]], equals(NA))
})

test_that("classification returns the correct class", {
	expect_that(clas_ncbi, is_a("classification"))
	expect_that(clas_ncbi[[1]], is_a("data.frame"))
	expect_that(length(clas_ncbi), equals(2))

  expect_that(clas_itis, is_a("classification"))
	expect_that(clas_itis[[1]], is_a("data.frame"))
  expect_that(length(clas_itis), equals(2))

# 	expect_that(clas_eol, is_a("list"))
# 	expect_that(clas_eol[[1]], is_a("data.frame"))
# 	expect_that(length(clas_eol), equals(2))

# 	expect_that(clas_col, is_a("list"))
# 	expect_that(clas_col[[1]], is_a("data.frame"))
# 	expect_that(length(clas_col), equals(2))

# 	expect_that(clas_tp, is_a("classification"))
# 	expect_that(clas_tp[[1]], is_a("data.frame"))
# 	expect_that(length(clas_tp), equals(2))
})

test_that("check S3-methods for tsn and uid class", {
  expect_identical(clas_uids, clas_ncbi)
  expect_equal(clas_tsns, clas_itis)
#   expect_identical(clas_eolids, clas_ncbi)
  #### FIX THESE TWO, SHOULD BE MATCHING
  # expect_identical(clas_colids, clas_col)
#   expect_identical(clas_tpids, clas_tp)
})

test_that("passing in an id works", {
  fromid_ncbi <- classification(9606, db = 'ncbi')
  fromid_itis <- classification(129313, db = 'itis')
  fromid_gbif <- classification(c(2704179, 2441176), db = 'gbif')
  fromid_nbn <- classification("NBNSYS0000004786", db = 'nbn')

  expect_is(fromid_ncbi, "classification")
  expect_equal(attr(fromid_ncbi, "db"), "ncbi")

  expect_is(fromid_itis, "classification")
  expect_equal(attr(fromid_itis, "db"), "itis")

  expect_is(fromid_gbif, "classification")
  expect_equal(attr(fromid_gbif, "db"), "gbif")

  expect_is(fromid_nbn, "classification")
  expect_equal(attr(fromid_nbn, "db"), "nbn")
})

test_that("rbind and cbind work correctly", {
  skip_on_cran()

  out <- get_ids(names = c("Puma concolor","Accipiter striatus"),
                 db = c('ncbi','itis'), verbose=FALSE)
  cl <- classification(out)

  # rbind
  clr <- rbind(cl)
  expect_is(clr, "data.frame")
  expect_named(clr, c("name", "rank", "id", "query", "db"))

  # cbind
  clc <- cbind(cl)
  expect_is(clc, "data.frame")
  expect_more_than(length(names(clc)), 50)
})

df <- theplantlist[sample(1:nrow(theplantlist), 50), ]
nn <- apply(df, 1, function(x) paste(x["genus"], x["sp"], collapse = " "))

test_that("works on a variety of names", {
	expect_that(classification(nn[1], db = "ncbi", verbose=FALSE), is_a("classification"))
	expect_that(classification(nn[2], db = "ncbi", verbose=FALSE), is_a("classification"))
})
