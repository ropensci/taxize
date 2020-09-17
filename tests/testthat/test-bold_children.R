test_that("bold_children", {
  skip_on_cran()
  vcr::use_cassette("bold_children", {
    # Momotus (genus): 3 children
    aa <- bold_children(id = 88899)
    # Momotus aequatorialis (species): no children
    bb <- bold_children(id = 115130)
    # Megachilinae (subfamily): 2 groups (tribes: 3, genera: 60)
    cc <- bold_children(id = 4962)
  })

  expect_is(aa, "list")
  expect_named(aa, "species")
  expect_is(aa[[1]], "data.frame")
  expect_named(aa[[1]], c("name", "id", "rank"))

  expect_is(bb, "list")
  expect_null(names(bb))
  expect_is(bb[[1]], "data.frame")
  expect_equal(NROW(bb[[1]]), 0)

  expect_is(cc, "list")
  expect_named(cc, c("tribe", "genus"))
  expect_is(cc[[1]], "data.frame")
  expect_is(cc[[2]], "data.frame")
  expect_named(cc[[1]], c("name", "id", "rank"))
  expect_named(cc[[2]], c("name", "id", "rank"))
})

test_that("bold_children fails well", {
  skip_on_cran()

  # no inputs
  expect_error(bold_children())
  # id length must == 1
  expect_error(bold_children(id = 1:3))

  vcr::use_cassette("bold_children_no_results", {
    x <- bold_children("Adfadfasfadsfasddfads")
  })
  expect_equal(NROW(x[[1]]), 0)
})
