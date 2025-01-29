context("iucn_summary: basic usage")
test_that("iucn_summary returns the correct value", {
  if (Sys.getenv("IUCN_REDLIST_KEY") == "") {
    skip("No IUCN api key so test not run.")
  }
  skip_on_cran()

  # vcr::use_cassette("iucn_summary", {
  #   temp <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
  # }, preserve_exact_body_bytes = TRUE, match_requests_on = c("method", "query"))
  temp <- iucn_summary(c("Panthera uncia", "Lynx lynx"))

  expect_is(temp, "iucn_summary")
  expect_equal(length(iucn_status(temp)), 2)
})

context("iucn_summary: gives expected result for lots of names")
test_that("iucn_summary gives expected result for lots of names", {
  if (Sys.getenv("IUCN_REDLIST_KEY") == "") {
    skip("No IUCN api key so test not run.")
  }
  skip_on_cran()

  # vcr::use_cassette("iucn_summary_other_egs", {
  #   aa <- iucn_summary("Abies koreana")
  #   bb <- iucn_summary("Xylopia collina")
  #   cc <- iucn_summary("Brugmansia versicolor")
  #   dd <- iucn_summary("Achatinella buddii")
  #   ee <- iucn_summary("Annona hystricoides")
  #   ff <- iucn_summary("Chamaecrista onusta")
  #   gg <- iucn_summary("Cyornis lemprieri")
  #   hh <- iucn_summary("Frailea pumila")
  # }, 
  #   preserve_exact_body_bytes = TRUE, 
  #   match_requests_on = c("method", "query")
  # )
  aa <- iucn_summary("Abies koreana")
  bb <- iucn_summary("Xylopia collina")
  cc <- iucn_summary("Brugmansia versicolor")
  dd <- iucn_summary("Achatinella buddii")
  ee <- iucn_summary("Annona hystricoides")
  ff <- iucn_summary("Chamaecrista onusta")
  gg <- iucn_summary("Cyornis lemprieri")
  hh <- iucn_summary("Frailea pumila")
  
  expect_equal(aa$`Abies koreana`$red_list_category$code, "EN")
  expect_equal(bb$`Xylopia collina`$red_list_category$code, "NT")
  expect_equal(cc$`Brugmansia versicolor`$red_list_category$code, "EW")
  expect_equal(dd$`Achatinella buddii`$red_list_category$code, "EX")
  expect_equal(ee$`Annona hystricoides`$red_list_category$code, "CR")
  expect_equal(ff$`Chamaecrista onusta`$red_list_category$code, "VU")
  expect_equal(gg$`Cyornis lemprieri`$red_list_category$code, "NT")
  expect_equal(hh$`Frailea pumila`$red_list_category$code, "LC")
})
