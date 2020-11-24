run_one_eg <- function(f, out_file = NULL, run_donttest = FALSE, run_dontrun = FALSE) {
  tmp <- tempfile(fileext = ".R")
  utils::capture.output(tools::Rd2ex(f, out = tmp,
    commentDontrun = !run_dontrun, commentDonttest = !run_donttest))
  if (!file.exists(tmp)) return(NULL) # no example
  utils::capture.output(suppressWarnings(suppressMessages(
    ret <- tryCatch(
      source(tmp, echo = TRUE, max.deparse.length = Inf),
      error = function (e) NULL)
  )), file = out_file, append = TRUE)
}

scott_run_examples <- function (pkg = ".", out_file = NULL, run_donttest = FALSE,
  run_dontrun = FALSE, document = TRUE, start = NULL) {

  pkg <- devtools::as.package(pkg)
  if (document) devtools::document(pkg)
  files <- devtools:::rd_files(pkg$path, start = start)
  if (length(files) == 0) return()
  cli::cat_rule(left = paste0("Running ", length(files), " example files"),
    right = pkg$package)
  devtools::load_all(pkg$path, reset = TRUE, export_all = FALSE)
  on.exit(devtools::load_all(pkg$path, reset = TRUE))
  lapply(files, run_one_eg, out_file = out_file, run_donttest = run_donttest,
    run_dontrun = run_dontrun)
  invisible()
}

# runegsafe <- plyr::failwith(NULL, pkgload::run_example)
# scott_run_examples <- function(pkg = ".", start = NULL, show = TRUE,
#   run_donttest = FALSE, run_dontrun = FALSE, fresh = FALSE,
#   document = TRUE) {
  
#   pkg <- devtools::as.package(pkg)
#   if (fresh) {
#     to_run <- eval(substitute(function() devtools::run_examples(pkg = path,
#       start = start, test = test, run = run, fresh = FALSE),
#       list(path = pkg$path, start = start, test = test,
#         run = run)))
#     callr::r(to_run, show = TRUE, spinner = FALSE)
#     return(invisible())
#   }
#   if (document) devtools::document(pkg)
#   if (!missing(show)) warning("`show` is deprecated", call. = FALSE)
#   files <- devtools:::rd_files(pkg$path, start = start)
#   if (length(files) == 0) return()
#   cli::cat_rule(left = paste0("Running ", length(files), " example files"),
#     right = pkg$package)
#   devtools::load_all(pkg$path, reset = TRUE, export_all = FALSE)
#   on.exit(devtools::load_all(pkg$path, reset = TRUE))
#   lapply(files, runegsafe, run_donttest = run_donttest,
#     run_dontrun = run_dontrun, quiet = FALSE)
#   invisible()
# }
