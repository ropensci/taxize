context("progressor")

taxize_options(FALSE, quiet = TRUE)

test_that("progressor works", {
  skip_on_cran()

  nms <- c("Quercus", "Sasdsfasdf")
  aa <- progressor$new(items = nms)

  expect_is(aa, "progressor")
  expect_is(aa$d, "integer")
  expect_is(aa$p, "numeric")
  expect_is(aa$completed, "function")
  expect_is(aa$completed_found, "function")
  expect_is(aa$completed_not_found, "function")
  expect_is(aa$prog_start, "function")
  expect_is(aa$prog, "function")
  expect_is(aa$prog_found, "function")
  expect_is(aa$prog_not_found, "function")
  expect_is(aa$prog_summary, "function")

  # start
  expect_output(aa$prog_start(), "2 queries")
  # expect_output(aa$prog_start(), "══")

  # completed
  expect_output(aa$prog_found(), "Found")
  aa$completed(nms[1], "found")
  expect_output(aa$prog_found(), "Quercus")
  
  aa$completed(nms[2], "not found")
  expect_output(aa$prog_not_found(), "Not Found")
  expect_output(aa$prog_not_found(), "Sasdsfasdf")
  
  # expect_output(aa$prog_summary(), "══")
  expect_output(aa$prog_summary(), "Results")
  expect_output(aa$prog_summary(), "Total: 2")
  expect_output(aa$prog_summary(), "Found: 1")
  expect_output(aa$prog_summary(), "Not Found: 1")
})

test_that("progressor fails well", {
  skip_on_cran()
  
  expect_error(progressor$new(apple = 5), "unused argument")
})

taxize_options(TRUE, quiet = TRUE)
