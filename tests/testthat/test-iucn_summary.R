context("iucn_summary")


test_that("iucn_summary returns the correct value", {
  skip_on_cran()

  temp <- iucn_summary(c("Panthera uncia", "Lynx lynx"))
  temp2 <- suppressWarnings(iucn_summary_id(c(22732, 12519)))

  expect_that(length(temp[[1]]), equals(4))

  expect_is(temp, "iucn_summary")

  #expect_equal(temp, temp2)

  expect_equal(length(iucn_status(temp)), 2)
})

test_that("iucn_summary gives expected result for lots of names", {
  skip_on_cran()

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
  skip_on_cran()

  ii <- suppressWarnings(iucn_summary_id(22685566, distr_detail = TRUE))
  expect_equal(names(ii$`22685566`$distr), c("Native", "Introduced"))
  expect_equal(vapply(ii$`22685566`$distr, length, 0),
               c(Native = 12, Introduced = 1))
})

test_that("iucn_summary and iucn_summary_id fail well", {
  skip_on_cran()

  expect_error(iucn_summary(""), "Not Found")
  #expect_equal(suppressWarnings(iucn_summary(""))[[1]]$status, NA)
  expect_warning(iucn_summary("Abies"), "not found")
  expect_warning(iucn_summary_id(0), "not found")
  expect_equal(suppressWarnings(iucn_summary_id(0))[[1]]$status, NA)
})
