context("lowest_common")

test_that("lowest_common works with ncbi, passing in classifications and doing internally", {
  id <- c("9031", "9823", "9606", "9470")
  idc <- classification(id, db = 'ncbi')
  aa <- lowest_common(id[2:4], db = "ncbi")
  bb <- lowest_common(id[2:4], db = "ncbi", low_rank = 'class')
  cc <- lowest_common(id[2:4], db = "ncbi", class_list = idc, low_rank = 'class')

	expect_is(aa, "data.frame")
  expect_is(bb, "data.frame")
  expect_is(cc, "data.frame")
  expect_named(aa, c('name', 'rank', 'id'))
  expect_named(cc, c('name', 'rank', 'id'))

  expect_identical(aa, bb)
  expect_identical(bb, cc)

  expect_equal(NROW(aa), 1)

  # passing in classification list obs. takes less time
  expect_lt(
    system.time(lowest_common(id[2:4], db = "ncbi", class_list = idc, low_rank = 'class'))[3],
    system.time(lowest_common(id[2:4], db = "ncbi", low_rank = 'class'))[3]
  )
})

test_that("lowest_common works with itis", {
  #lowest_common(spp, db = "itis")
  #spp <- c("Sus scrofa", "Homo sapiens", "Nycticebus coucang")

  ids <- c("180722","180092","572890")
  idc <- classification(ids, db = 'itis')

  expect_identical(
    lowest_common(ids, db = "itis"),
    lowest_common(ids, db = "itis", class_list = idc)
  )

  bb <- lowest_common(ids, db = "itis", low_rank = 'class')
  cc <- lowest_common(ids, db = "itis", class_list = idc, low_rank = 'class')

  expect_is(bb, "data.frame")
  expect_is(cc, "data.frame")
  expect_named(cc, c('name', 'rank', 'id'))

  expect_identical(bb, cc)

  expect_equal(NROW(bb), 1)
})
