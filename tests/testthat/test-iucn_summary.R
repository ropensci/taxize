context("iucn_summary: basic usage")
test_that("iucn_summary returns the correct value", {
  if (Sys.getenv("IUCN_REDLIST_KEY") == "") {
    skip("No IUCN api key so test not run.")
  }

  vcr::use_cassette("iucn_summary", {
    temp <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
  }, preserve_exact_body_bytes = TRUE)

  expect_that(length(temp[[1]]), equals(4))
  expect_is(temp, "iucn_summary")
  expect_equal(length(iucn_status(temp)), 2)
})

context("iucn_summary: gives expected result for lots of names")
test_that("iucn_summary gives expected result for lots of names", {
  if (Sys.getenv("IUCN_REDLIST_KEY") == "") {
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
  }, preserve_exact_body_bytes = TRUE)

  expect_equal(aa$`Abies koreana`$status, "EN")
  expect_equal(bb$`Xylopia collina`$status, "EN")
  expect_equal(cc$`Brugmansia versicolor`$status, "EW")
  expect_equal(dd$`Achatinella buddii`$status, "EX")
  expect_equal(ee$`Annona hystricoides`$status, "CR")
  expect_equal(ff$`Chamaecrista onusta`$status, "VU")
  expect_equal(gg$`Cyornis lemprieri`$status, "NT")
  expect_equal(hh$`Frailea pumila`$status, "LC")
})

context("iucn_summary: curl options work")
test_that("iucn_summary curl options work", {
  skip_on_cran()
  if (Sys.getenv("IUCN_REDLIST_KEY") == "") {
    skip("No IUCN api key so test not run.")
  }
  expect_error(iucn_summary("Abies koreana", timeout_ms = 1, messages = FALSE))
})
