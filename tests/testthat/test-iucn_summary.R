context("iucn_summary")

temp <- iucn_summary(c("Panthera uncia", "Lynx lynx"))

test_that("iucn_summary returns the correct value", {
  expect_that(length(temp[[1]]), equals(4))
})

test_that("iucn_summary returns the correct class", {
  expect_that(temp, is_a("iucn"))
})

test_that("iucn_status", {
  expect_that(length(iucn_status(temp)), equals(2))
})

test_that("iucn_summary gives expected result for lots of names", {
  aa <- iucn_summary("Abies koreana")
  expect_equal(aa$`Abies koreana`$status, "EN")

  bb <- iucn_summary("Xylopia collina")
  expect_equal(bb$`Xylopia collina`$status, "EN")

  cc <- iucn_summary("Brugmansia versicolor")
  expect_equal(cc$`Brugmansia versicolor`$status, "EW")

  dd <- iucn_summary("Achatinella buddii")
  expect_equal(dd$`Achatinella buddii`$status, "EX")

  ee <- iucn_summary("Annona hystricoides")
  expect_equal(ee$`Annona hystricoides`$status, "CR")

  ff <- iucn_summary("Chamaecrista onusta")
  expect_equal(ff$`Chamaecrista onusta`$status, "VU")

  gg <- iucn_summary("Cyornis lemprieri")
  expect_equal(gg$`Cyornis lemprieri`$status, "NT")

  hh <- iucn_summary("Frailea pumila")
  expect_equal(hh$`Frailea pumila`$status, "LC")
})

test_that("iucn_summary fails well", {
  expect_warning(iucn_summary(""), "not found")
  expect_equal(suppressWarnings(iucn_summary(""))[[1]]$status, NA)
  expect_warning(iucn_summary("Abies"), "not found")
})
