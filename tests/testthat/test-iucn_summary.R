context("iucn_summary")

test_that("iucn_summary returns the correct value", {
  if (Sys.getenv('IUCN_REDLIST_KEY') == "") {
    skip("No IUCN api key so test not run.")
  }

  vcr::use_cassette("iucn_summary", {
    temp <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
    temp2 <- suppressWarnings(iucn_summary_id(c(22732, 12519)))
  })

  expect_that(length(temp[[1]]), equals(4))
  expect_is(temp, "iucn_summary")
  expect_equal(length(iucn_status(temp)), 2)
})

test_that("iucn_summary gives expected result for lots of names", {
  if (Sys.getenv('IUCN_REDLIST_KEY') == "") {
    skip("No IUCN api key so test not run.")
  }

  vcr::use_cassette("iucn_summary_other_egs", {
    aa <- iucn_summary("Abies koreana")
    bb <- iucn_summary("Xylopia collina")
    cc <- iucn_summary("Brugmansia versicolor")
    dd <- iucn_summary("Achatinella buddii")
    ee <- iucn_summary("Annona hystricoides")
    ff <- iucn_summary("Chamaecrista onusta")
    gg <- iucn_summary("Cyornis lemprieri")
    hh <- iucn_summary("Frailea pumila")
  })
  
  expect_equal(aa$`Abies koreana`$status, "EN")
  expect_equal(bb$`Xylopia collina`$status, "EN")
  expect_equal(cc$`Brugmansia versicolor`$status, "EW")
  expect_equal(dd$`Achatinella buddii`$status, "EX")
  expect_equal(ee$`Annona hystricoides`$status, "CR")
  expect_equal(ff$`Chamaecrista onusta`$status, "VU")
  expect_equal(gg$`Cyornis lemprieri`$status, "NT")
  expect_equal(hh$`Frailea pumila`$status, "LC")
})

test_that("iucn_summary_id with distr_detail produces correct output", {
  skip_on_cran()
  if (Sys.getenv('IUCN_REDLIST_KEY') == "") {
    skip("No IUCN api key so test not run.")
  }

  ii <- suppressWarnings(iucn_summary_id(22685566, distr_detail = TRUE))
  expect_equal(names(ii$`22685566`$distr), c("Native", "Introduced"))
  expect_equal(vapply(ii$`22685566`$distr, length, 0),
               c(Native = 12, Introduced = 1))
})

test_that("iucn_summary and iucn_summary_id fail well", {
  if (Sys.getenv('IUCN_REDLIST_KEY') == "") {
    skip("No IUCN api key so test not run.")
  }

  vcr::use_cassette("iucn_summary_not_found", {
    expect_error(iucn_summary(""), "Not Found")
    expect_warning(iucn_summary("Abies"), "not found")
    expect_warning(iucn_summary_id(0), "not found")
    expect_equal(suppressWarnings(iucn_summary_id(0))[[1]]$status, NA)
  })
})
