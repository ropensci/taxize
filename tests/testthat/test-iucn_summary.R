context("iucn_summary")

temp <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
temp2 <- iucn_summary_id(c(22732, 12519))

test_that("iucn_summary returns the correct value", {
  expect_that(length(temp[[1]]), equals(4))
})

test_that("iucn_summary returns the correct class", {
  expect_that(temp, is_a("iucn"))
})

test_that("iucn_summary and iucn_summary_id have the same output", {
  expect_identical(temp, temp2)
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

test_that("iucn_summary_id with distr_detail produces correct output", {
  ii <- iucn_summary_id(22685566, distr_detail = TRUE)
  expect_equal(names(ii$`Ara chloropterus`$distr), c("Native", "Introduced"))
  expect_equal(vapply(ii$`Ara chloropterus`$distr, length, 0), 
               c(Native = 12, Introduced = 1))
})

test_that("iucn_summary and iucn_summary_id fail well", {
  expect_warning(iucn_summary(""), "not found")
  expect_equal(suppressWarnings(iucn_summary(""))[[1]]$status, NA)
  expect_warning(iucn_summary("Abies"), "not found")
  expect_warning(iucn_summary_id(0), "not found")
  expect_equal(suppressWarnings(iucn_summary_id(0))[[1]]$status, NA)
})
