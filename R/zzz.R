con_utf8 <- function(x) content(x, "text", encoding = "UTF-8")

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

collapse <- function(x, fxn, class, match=TRUE, ...){
  tmp <- lapply(x, fxn, ...)
  if (match) {
    structure(sapply(tmp, unclass), class = class,
              match = sapply(tmp, attr, which = "match"),
              multiple_matches = sapply(tmp, attr, which = "multiple_matches"),
              pattern_match = sapply(tmp, attr, which = "pattern_match"),
              uri = tcnull(sapply(tmp, attr, which = "uri")))
  } else {
    structure(sapply(tmp, unclass), class = class,
              uri = tcnull(sapply(tmp, attr, which = "uri")))
  }
}

make_generic <- function(x, uu, clz, check=TRUE){
  if (check) {
    if ( evalfxn(clz)(x) ) {
      toid(x, uu, clz)
    } else {
      structure(NA, class = clz, match = "not found", multiple_matches = FALSE, pattern_match = FALSE, uri = NA)
    }
  } else {
    toid(x, uu, clz)
  }
}

evalfxn <- function(x) eval(parse(text = paste0("check", "_", x)))

toid <- function(x, url, class){
  uri <- sprintf(url, x)
  structure(x, class = class, match = "found", multiple_matches = FALSE, pattern_match = FALSE, uri = uri)
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

check_rows <- function(x){
  stopifnot(is.numeric(x) || any(is.na(x)))
  x
}

sub_rows <- function(x, rows){
  rows <- check_rows(rows)
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

nstop <- function(x, arg='db') if (is.null(x)) stop(sprintf("Must specify %s!", arg), call. = FALSE)

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

# function to help filter get_*() functions for a rank name or rank itself --------------
filt <- function(df, rank, z) {
  if (NROW(df) == 0) {
    df
  } else {
    if (!is.null(z)) {
      #mtch <- grep(tolower(z), tolower(df[,rank]))
      mtch <- grep(sprintf("%s", tolower(z)), tolower(df[,rank]))
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

# failwith replacment ------------------
try_default <- function(expr, default, quiet = FALSE){
  result <- default
  if (quiet) {
    tryCatch(result <- expr, error = function(e) {
    })
  }
  else {
    try(result <- expr)
  }
  result
}

failwith <- function(default = NULL, f, quiet = FALSE){
  f <- match.fun(f)
  function(...) try_default(f(...), default, quiet = quiet)
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
  if (!is.null(x)) {
    if (!class(x) %in% y) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

dt2df <- function(x) {
  (data.table::setDF(
    data.table::rbindlist(x, use.names = TRUE, fill = TRUE, idcol = TRUE)))
}
