mssg <- function(v, ...) if (v) message(...)
tc <- function(l) Filter(Negate(is.null), l)
tcnull <- function(x) if (all(sapply(x, is.null))) NULL else x

pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

# pluck with unname
pluck_un <- function(x, name, type) unname(pluck(x, name, type))

collapse <- function(x, fxn, class, match=TRUE, ...) {
  tmp <- lapply(x, fxn, ...)
  if (match) {
    structure(sapply(tmp, unclass), class = class,
              name = unlist(sapply(tmp, attr, which = "name")),
              match = sapply(tmp, attr, which = "match"),
              multiple_matches = sapply(tmp, attr, which = "multiple_matches"),
              pattern_match = sapply(tmp, attr, which = "pattern_match"),
              uri = tcnull(sapply(tmp, attr, which = "uri")))
  } else {
    structure(sapply(tmp, unclass), class = class,
              uri = tcnull(sapply(tmp, attr, which = "uri")))
  }
}

evalfxn <- function(x) eval(parse(text = paste0("check", "_", x)))

toid <- function(x, url, class, ...) {
  uri <- sprintf(url, x)
  structure(x, class = class, match = "found", multiple_matches = FALSE,
            pattern_match = FALSE, uri = uri, ...)
}

add_uri <- function(ids, url, z = NULL){
  if ( !all(is.na(ids)) ) {
    uri_ids <- if (!is.null(z)) z else ids
    attr(ids, 'uri') <- sapply(uri_ids, function(x) {
      if (!is.na(x)) sprintf(url, x) else NA
    }, USE.NAMES = FALSE)
  }
  ids
}

assert_rows <- function(rows) {
  if (!all(is.na(rows))) {
    assert(rows, c("numeric", "integer"))
    stopifnot(all(rows > 0))
  }
}

check_rows <- function(z) {
  if (!is.numeric(z) && !any(is.na(z))) {
    stop("'rows' must be numeric or NA", call. = FALSE)
  }
  if (is.numeric(z)) {
    if (length(z) == 1) {
      if (z < 1) {
        stop("'rows' value must be an integer 1 or greater",
             call. = FALSE)
      }
    }
  }
}

sub_rows <- function(x, rows){
  check_rows(rows)
  if ( any(is.na(rows)) ) {
    x
  } else {
    # subset
    if (NROW(x) == 0) {
      x
    } else {
      # check that vector is = or > nrow of data.frame
      if (NROW(x) < max(rows)) rows <- min(rows):NROW(x)
      x[rows, ]
    }
  }
}

sub_vector <- function(x, rows){
  rows <- check_rows(rows)
  if (length(x) < max(rows)) rows <- min(rows):length(x)
  if ( any(is.na(rows)) ) x else x[rows]
}

nstop <- function(x, arg='db') {
  if (is.null(x)) stop(sprintf("Must specify %s!", arg), call. = FALSE)
}

colClasses <- function(d, colClasses) {
  colClasses <- rep(colClasses, len=length(d))
  d[] <- lapply(seq_along(d), function(i) switch(colClasses[i],
    numeric=as.numeric(d[[i]]),
    character=as.character(d[[i]]),
    Date=as.Date(d[[i]], origin='1970-01-01'),
    POSIXct=as.POSIXct(d[[i]], origin='1970-01-01'),
    factor=as.factor(d[[i]]),
    as(d[[i]], colClasses[i]) ))
  d
}

strtrim <- function(str) {
  gsub("^\\s+|\\s+$", "", str)
}

# function to help filter get_*() functions for a rank name or rank itself ----
filt <- function(df, rank, z) {
  if (NROW(df) == 0) {
    df
  } else {
    if (!is.null(z)) {
      mtch <- grep(sprintf("^%s$", tolower(z)), tolower(df[,rank]))
      if (length(mtch) != 0) {
        df[mtch, ]
      } else {
        data.frame(NULL)
      }
    } else {
      df
    }
  }
}

argsnull <- function(x) {
  if (length(x) == 0) {
    NULL
  } else {
    x
  }
}

as_l <- function(z) {
  if (is.logical(z) || tolower(z) == "true" || tolower(z) == "false") {
    if (z) {
      return('true')
    } else {
      return('false')
    }
  } else {
    return(z)
  }
}

should_be <- function(arg_name, x, class) {
  if (!is(x, class)) {
    stop(sprintf("'%s' should be of class '%s'", arg_name, class), call. = FALSE)
  }
}

move_col <- function(tt, y){
  tt[ c(names(tt)[ -sapply(y, function(m) grep(m, names(tt))) ], y) ]
}

move_col2 <- function(x, y) x[ c(names(x)[-grep(y, names(x))], y) ]

move_col_begin <- function(tt, y){
  tt[ c(y, names(tt)[ -sapply(y, function(m) grep(m, names(tt))) ]) ]
}

pop <- function(x, nms) {
  x[ !names(x) %in% nms ]
}

assert <- function(x, y) {
  if (!is.null(x) && !is_na(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

is_na <- function(x) {
  if (is.list(x)) return(FALSE)
  if (is.environment(x)) return(FALSE)
  return(all(is.na(x)))
}

assert_state <- function(x, y) {
  if (!is.null(x) && inherits(x, "taxon_state")) {
    if (x$class != y) {
      stop("taxon_state class must match the get_* function called ",
        call. = FALSE)
    }
  }
}

dt2df <- function(x, idcol = TRUE) {
  (data.table::setDF(
    data.table::rbindlist(x, use.names = TRUE, fill = TRUE, idcol = idcol)))
}

dt2tibble <- function(x) {
  tibble::as_tibble(data.table::setDF(
    data.table::rbindlist(x, use.names = TRUE, fill = TRUE))
  )
}

dbswap <- function(x) {
  switch(
    x,
    boldid = "bold",
    eolid = "eol",
    gbifid = "gbif",
    natservid = "natserv",
    nbnid = "nbn",
    tolid = "tol",
    tpsid = "tropicos",
    tsn = "itis",
    uid = "ncbi",
    wormsid = "worms",
    stop("'db' not recognized", call. = FALSE)
  )
}

check_entrez_key <- function (x) {
  tmp <- if (is.null(x)) Sys.getenv("ENTREZ_KEY", "") else x
  if (tmp == "") {
    getOption("entrez_key",
      warning("no API key found for Entrez, proceeding w/o key",
        call. = FALSE))
  } else {
    tmp
  }
}

taxize_ua <- function(x) {
  versions <- c(
    `r-curl` = as.character(utils::packageVersion("curl")),
    crul = as.character(utils::packageVersion("crul")),
    taxize = as.character(utils::packageVersion("taxize"))
  )
  paste0(names(versions), "/", versions, collapse = " ")
}

`%||%` <- function (x, y) if (is.null(x) || length(x) == 0) y else x

# a common set of functions used together, so a helper fxn to do that
xml_text_all <- function(x, xpath) {
  xml2::xml_text(xml2::xml_find_all(x, xpath))
}

tx_ua <- function() {
  versions <- c(paste0("r-curl/", utils::packageVersion("curl")),
    paste0("crul/", utils::packageVersion("crul")),
    sprintf("rOpenSci(taxize/%s)", utils::packageVersion("taxize")))
  paste0(versions, collapse = " ")
}

tx_ual <- list(`User-Agent` = tx_ua(), `X-USER-AGENT` = tx_ua())

warn_db <- function(x, type) {
  if ("db" %in% names(x)) {
    if (!is.null(x$db)) {
      if (x$db != type) {
      warning(
      sprintf("'db' value '%s' ignored; does not match dispatched method '%s'",
            x$db, type),
          call. = FALSE, immediate. = TRUE)
      }
    }
  }
}

strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
strexec <- function(str, pattern) regmatches(str, regexec(pattern, str))


ncbi_rate_limit_pause <- function(key) {
  # NCBI limits requests to three per second when no key and ten per second with key
  ncbi_sleep <- taxize_env$options$ncbi_sleep_sec
  if (is.null(key)) {
    tmz <- c(ncbi_sleep, 0.334)
    time_sleep <- tmz[which.max(tmz)]
    Sys.sleep(time_sleep)
  } else {
    tmz <- c(ncbi_sleep, 0.101)
    time_sleep <- tmz[which.max(tmz)]
    Sys.sleep(tmz)
  }
}

# foo <- function(x) {
#   deparse(substitute(x))
# }
# bar <- function(z) {
#   foo(z)
# }
# bar(z = "adf")
taxize_ck <- conditionz::ConditionKeeper$new(times = 1, condition = "warning")
pchk <- function(from, to, pkg_version = "v0.9.97") {
  assert(deparse(substitute(from)), "character")
  assert(to, "character")
  assert(pkg_version, "character")
  # & will be removed in a future version
  param_mssg <- "`%s` param is deprecated as of taxize %s, use `%s` instead."
  parms_help <- "\nSee `?taxize-params` for more information."
  once_per <- "\nThis warning will be thrown once per R session."
  mssg <- c(sprintf(param_mssg, deparse(substitute(from)), pkg_version, to),
    parms_help, once_per)
  if (!is.null(from))
    taxize_ck$handle_conditions(warning(mssg))
}
