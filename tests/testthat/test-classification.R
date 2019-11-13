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
#   expect_is(classification(c("Chironomus riparius", "aaa vva"), db = 'itis', messages = FALSE), "classification")
# }

# eolids <- get_tsn(c("Chironomus riparius", "aaa vva"), messages = FALSE)
# colids <- get_colid(c("Chironomus riparius", "aaa vva"), messages = FALSE)
# tpsids <- get_tpsid(sciname=c("Helianthus excubitor", "aaa vva"), messages = FALSE)
# clas_eolids <- classification(eolids, messages = FALSE)
# clas_colids <- classification(colids)
# clas_tpids <- classification(tpsids, messages = FALSE)

# clas_eol <- classification(c("Helianthus petiolaris Nutt.", "aaa vva"), db = 'eol')
# names(clas_eol) <- NULL

# clas_col <- suppressMessages(classification(c("Puma concolor", "aaa vva"), db = 'col'))
# names(clas_col) <- NULL
# colids <- get_colid(c("Puma concolor", "aaa vva"), messages = FALSE)
# clas_colids <- classification(colids)
# names(clas_colids) <- NULL

# clas_tp <- suppressMessages(classification(c("Helianthus excubitor", "aaa vva"), db = 'tropicos'))
# names(clas_tp) <- NULL

test_that("classification returns the correct values and classes", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("classification", {
    clas_ncbi <- classification(c("Chironomus riparius", "aaa vva"), db = 'ncbi',
                                messages = FALSE)
    names(clas_ncbi) <- NULL

    clas_itis <- classification(c("Chironomus riparius", "aaa vva"), db = 'itis',
                                messages = FALSE)
    names(clas_itis) <- NULL
  })

	expect_that(clas_ncbi[[2]], equals(NA))
	expect_that(clas_itis[[2]], equals(NA))
# 	expect_that(clas_eol[[2]], equals(NA))
# 	expect_that(clas_col[[2]], equals(NA))
# 	expect_that(clas_tp[[2]], equals(NA))

	expect_is(clas_ncbi, "classification")
	expect_is(clas_ncbi[[1]], "data.frame")
	expect_equal(length(clas_ncbi), 2)

  expect_is(clas_itis, "classification")
	expect_is(clas_itis[[1]], "data.frame")
  expect_equal(length(clas_itis), 2)

# 	expect_that(clas_eol, is_a("list"))
# 	expect_that(clas_eol[[1]], is_a("data.frame"))
# 	expect_that(length(clas_eol), equals(2))

# 	expect_that(clas_col, is_a("list"))
# 	expect_that(clas_col[[1]], is_a("data.frame"))
# 	expect_that(length(clas_col), equals(2))

# 	expect_that(clas_tp, is_a("classification"))
# 	expect_that(clas_tp[[1]], is_a("data.frame"))
# 	expect_that(length(clas_tp), equals(2))

  vcr::use_cassette("classification_get_fxn", {
    uids <- get_uid(c("Chironomus riparius", "aaa vva"), messages = FALSE)
    tsns <- get_tsn(c("Chironomus riparius", "aaa vva"), messages = FALSE)
    clas_uids <- classification(uids, messages = FALSE)
    names(clas_uids) <- NULL
    clas_tsns <- classification(tsns, messages = FALSE)
    names(clas_tsns) <- NULL
  })
  
  expect_identical(clas_uids, clas_ncbi)
  expect_equal(clas_tsns, clas_itis)
#   expect_identical(clas_eolids, clas_ncbi)
  #### FIX THESE TWO, SHOULD BE MATCHING
  # expect_identical(clas_colids, clas_col)
#   expect_identical(clas_tpids, clas_tp)
})

test_that("passing in an id works", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("classification_passing_id", {
    fromid_ncbi <- classification(9606, db = 'ncbi')
    fromid_itis <- classification(129313, db = 'itis')
    fromid_gbif <- classification(c(2704179, 2441176), db = 'gbif')
  })
  #fromid_nbn <- classification("NBNSYS0000004786", db = 'nbn')

  expect_is(fromid_ncbi, "classification")
  expect_equal(attr(fromid_ncbi, "db"), "ncbi")

  expect_is(fromid_itis, "classification")
  expect_equal(attr(fromid_itis, "db"), "itis")

  expect_is(fromid_gbif, "classification")
  expect_equal(attr(fromid_gbif, "db"), "gbif")

  #expect_is(fromid_nbn, "classification")
  #expect_equal(attr(fromid_nbn, "db"), "nbn")
})

test_that("rbind and cbind work correctly", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("classification_cbind_rbind", {
    out <- get_ids(names = c("Puma concolor", "Accipiter striatus"),
                   db = 'ncbi', messages = FALSE)
    cl <- classification(out)
  })

  # rbind
  clr <- rbind(cl)
  expect_is(clr, "data.frame")
  expect_named(clr, c("name", "rank", "id", "query", "db"))

  # cbind
  clc <- cbind(cl)
  expect_is(clc, "data.frame")
  expect_gt(length(names(clc)), 50)
})

df <- theplantlist[sample(1:nrow(theplantlist), 50), ]
nn <- apply(df, 1, function(x) paste(x["genus"], x["species"], collapse = " "))

test_that("works on a variety of names", {
  skip_on_cran()
  
  x <- classification(nn[1], db = "ncbi", messages = FALSE)
  z <- classification(nn[2], db = "ncbi", messages = FALSE)

	expect_is(x, "classification")
	expect_is(z, "classification")
})

test_that("queries with no results fail well", {
  vcr::use_cassette("classification_no_results", {
    aa <- classification(x = "foobar", db = "itis", messages = FALSE)
    bb <- classification(get_tsn("foobar", messages = FALSE), messages = FALSE)
  })

  expect_true(is.na(unclass(aa)[[1]]))
  expect_identical(unname(aa), unname(bb))
})

test_that("all rank character strings are lower case (all letters)", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("classification_rank_is_lowercase", {
    aa <- classification(9606, db = 'ncbi', messages = FALSE)
    bb <- classification(129313, db = 'itis', messages = FALSE)
    #cc <- classification(57361017, db = 'eol', messages = FALSE)
    dd <- classification(2441176, db = 'gbif', messages = FALSE)
    #ee <- classification(25509881, db = 'tropicos', messages = FALSE)
    #ff <- classification("NBNSYS0000004786", db = 'nbn', messages = FALSE)
    gg <- classification("Chironomus riparius", db = 'col', messages = FALSE)
  })

  expect_false(all(grepl("[A-Z]", aa[[1]]$rank)))
  expect_false(all(grepl("[A-Z]", bb[[1]]$rank)))
  #expect_false(all(grepl("[A-Z]", cc[[1]]$rank)))
  expect_false(all(grepl("[A-Z]", dd[[1]]$rank)))
  #expect_false(all(grepl("[A-Z]", ee[[1]]$rank)))
  #expect_false(all(grepl("[A-Z]", ff[[1]]$rank)))
  expect_false(all(grepl("[A-Z]", gg[[1]]$rank)))
})


test_that("rows parameter, when used, works", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("classification_rows_param", {
    a <- classification("Asdfafsfd", db = 'ncbi', rows = 1, messages = FALSE)
    b <- classification("Asdfafsfd", db = 'itis', rows = 1, messages = FALSE)
    d <- classification("Asdfafsfd", db = 'gbif', rows = 1, messages = FALSE)
    e <- classification("Asdfafsfd", db = 'eol', rows = 1, messages = FALSE)
    f <- classification("Asdfafsfd", db = 'col', rows = 1, messages = FALSE)
    g <- classification("Asdfafsfd", db = 'tropicos', rows = 1, messages = FALSE)
    h <- classification("Asdfafsfd", db = 'nbn', rows = 1, messages = FALSE)
  })

  expect_is(a, "classification")
  expect_is(b, "classification")
  expect_is(d, "classification")
  expect_is(e, "classification")
  expect_is(f, "classification")
  expect_is(g, "classification")
  expect_is(h, "classification")
})

test_that("warn on mismatch 'db'", {
  vcr::use_cassette("classification_warn_on_db_mismatch", {
    expect_warning(
      classification(
        get_uid("Chironomus riparius", messages = FALSE), db = "itis"))
  })
})
