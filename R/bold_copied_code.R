# NOTE: The code in this file was copied from the bold R package in order to circumvent the bold package being taken off CRAN.
#    If the bold package is put back on CRAN, this code will be deleted and the offical bold package will be used instead.
#    The code has been copied under the following license:
#
# MIT License
# 
# Copyright (c) 2021 bold authors
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


#' Search BOLD for taxonomy data by BOLD ID.
#'
#' @keywords internal
bold_tax_id <- function(id, dataTypes = "basic", includeTree = FALSE,
                        response = FALSE, ...) {
  .Deprecated("bold_tax_id2")
  #-- arguments check
  if (missing(id)) stop("argument 'id' is missing, with no default")
  if (!grepl("true|false", includeTree, ignore.case = TRUE))
    warning("'includeTree' should be either TRUE or FALSE.")
  if (!inherits(id, c("character", "numeric", "integer")))
    warning("'id' should be of class character, numeric or integer.")
  #-- make sure user have correct data types
  dataTypes <- .check_dataTypes_dep(dataTypes)
  if (!nzchar(dataTypes)) {
    out <- data.frame(input = id, noresults = NA)
  } else {
    #-- prep query parameters
    params <- list(dataTypes = dataTypes, includeTree = tolower(includeTree))
    #-- make URL
    URL <- b_url("API_Tax/TaxonData")
    #-- fetch data from the api
    res <- lapply(`names<-`(id, id), function(x) {
      # no need to do the call if id is NA
      if (is.na(x))
        data.frame(input = NA, noresults = NA)
      else
        .get_response_dep(args = c(taxId = x, params), url = URL, ...)
    })
    if (response) {
      out <- res
    } else {
      out <- b_rbind(mapply(FUN = .process_response_dep, x = res, y = id,
                            MoreArgs = list(z = includeTree, w = dataTypes),
                            SIMPLIFY = FALSE))
      if (NCOL(out) == 1) {
        out$noresults <- NA
      }
    }
  }
  out
}
.process_response_dep <- function(x, y, z, w){
  if (is.data.frame(x)) return(x)
  tt <- rawToChar(x$content)
  out <- if (x$status_code > 202) "stop" else jsonlite::fromJSON(tt)
  if ( length(out) == 0 || identical(out[[1]], list()) || any(out == "stop") ) {
    data.frame(input = y, stringsAsFactors = FALSE)
  } else {
    if (w %in% c("stats",'images','geo','sequencinglabs','depository')) out <- out[[1]]
    trynames <- tryCatch(as.numeric(names(out)), warning = function(w) w)
    if (!inherits(trynames, "simpleWarning")) names(out) <- NULL
    if (any(vapply(out, function(x) is.list(x) && length(x) > 0, logical(1)))) {
      out <- lapply(out, function(x) Filter(length, x))
    } else {
      out <- Filter(length, out)
    }
    if (!is.null(names(out))) {
      df <- data.frame(out, stringsAsFactors = FALSE)
    } else {
      df <- b_rbind(lapply(out, data.frame, stringsAsFactors = FALSE))
    }
    row.names(df) <- NULL
    if ("parentid" %in% names(df)) df <- df[order(df[,"parentid"]),]
    row.names(df) <- NULL
    data.frame(input = y, df, stringsAsFactors = FALSE)
  }
}
.get_response_dep <- function(args, url, ...){
  cli <- crul::HttpClient$new(url = url)
  out <- cli$get(query = args, ...)
  out$raise_for_status()
  stopifnot(out$headers$`content-type` == 'text/html; charset=utf-8')
  return(out)
}
.check_dataTypes_dep <- function(x){
  x <- b_split(x, ",", fixed = TRUE, simplify = TRUE)
  # corrects for the json typo in case the option is taken from a previous query
  x[x == "depositories"] <- "depository"
  if (length(x) > 1 && any(x == "all")) {
    x <- "all"
  } else {
    wrongType <- !x %in% b_dataTypes
    if (any(wrongType)) {
      warning(b_ennum(x[wrongType], quote = TRUE),
              if (sum(wrongType) > 1) " are not valid data types"
              else " is not a valid data type",
              if (!all(wrongType)) " and will be skipped." else ".",
              "\nChoices are:",
              b_ennum(b_dataTypes, "or", TRUE))
      x <- x[!wrongType]
    }
    x <- paste(x, collapse = ",")
  }
  x
}

#' Search BOLD for taxonomy data by BOLD ID.
#'
#' @keywords internal
bold_tax_id2 <- function(id, dataTypes = 'basic', includeTree = FALSE,
                         response = FALSE, ...) {
  #-- arguments check
  if (missing(id)) stop("argument 'id' is missing, with no default")
  # no need to do the call if id is NA
  if (length(id) == 1 && is.na(id))
    return(data.frame(input = NA, taxid = NA))
  b_assert_logical(includeTree)
  b_assert(dataTypes, "character")
  if (is.list(id)) {
    lapply(id, b_assert, what = c("character", "numeric", "integer"), name = "id")
  } else {
    b_assert(id, c("character", "numeric", "integer"))
  }
  # for compatibility with bold_tax_id
  if (length(dataTypes) == 1 && grepl(",", dataTypes)) {
    dataTypes <- c(b_split(dataTypes, ",", fixed = TRUE, simplify = TRUE))
  }
  dataTypes <- b_assert_dataTypes(dataTypes)
  #-- prep query params
  params <- list(
    dataTypes = paste(dataTypes, collapse = ","),
    includeTree = tolower(includeTree)
  )
  #-- fetch data from the api
  res <- lapply(b_nameself(id), function(x) {
    if (!length(x) || !nzchar(x)) {
      list(response = NULL, warning = "id was empty")
    } else if (is.na(x) || grepl("^NA?$", x)) {
      list(response = NULL, warning = "id was NA")
    } else {
      r <- b_GET(query = c(taxId = x, params), api = "API_Tax/TaxonData",
                 check = TRUE, ...)
      if (nzchar(r$warning) || r$response$status_code > 202) {
        w <- xml2::read_html(r$response$content)
        w <- b_lines(xml2::xml_text(w))
        w <- w[b_detect(w, "Fatal")]
        if (b_detect(w, "non-object")) w <- c(w, "Request returned no match.")
        r$warning <- c(r$warning, w)
        r$response <- NULL
      }
      r
    }
  })
  if (response) {
    res
  } else {
    #-- make data.frame with the response(s)
    #-- fixing dataTypes to match bold response
    if (length(dataTypes) == 1 && dataTypes == "all") {
      dataTypes <- b_dataTypes[1:7]
    }
    dataTypes[dataTypes == "geo"] <- "country"
    #-- parsing bold response by id
    out <- mapply(
      b_process_tax_id,
      res,
      ids = id,
      MoreArgs = list(types = dataTypes, tree = includeTree),
      SIMPLIFY = FALSE
    )
    out <- b_format_tax_id_output(out, types = dataTypes, tree = includeTree)
    # -- add attributes to output
    w <- lapply(res, `[[`, "warning")
    attr(out, "errors") <- b_rm_empty(w)
    attr(out, "params") <- list(dataTypes = dataTypes,
                                includeTree = includeTree)
    out
  }
}
b_process_tax_id <- function(x, ids, types, tree) {
  if (length(x$response)) {
    out <- b_parse(x$response, format = "json", raise = FALSE)
    if (!length(out) || identical(out[[1]], list())) {
      data.frame(taxid = NA)
    } else {
      if (!tree) {
        out <- b_format_tax_id(out, ids = ids)
      } else {
        tmp <-
          lapply(b_nameself(names(out)), function(id) b_format_tax_id(out[[id]], id))
        out <-
          lapply(b_nameself(types), b_grp_dataTypes_tree, x = tmp)
        if ("basic" %in% names(out) &&
            "parentid" %in% names(out[["basic"]])) {
          out[["basic"]] <-
            out[["basic"]][order(out[["basic"]][["parentid"]]), ]
        }
      }
      out
    }
  }
}
b_format_tax_id <- function(x, ids) {
  # 'geo' isn't group like the others for some reason
  # it's split between country and site map, default
  # was to return only 'country' (although I'm not sure why),
  # so removing the sitemap
  x$sitemap <- NULL
  # some dataTypes returns data.frames or long lists
  # simplifying all of those would make the data frame very large.
  # so adjusting for the different types :
  basic.nms <- which(names(x) %in% c("taxid", "taxon", "tax_rank", "tax_division",
                                     "parentid", "parentname", "taxonrep"))
  wiki.nms <- which(names(x) %in% c("wikipedia_summary", "wikipedia_link"))
  if (length(x$stats)) {
    x$stats <- b_format_tax_id_stats(x$stats)
  }
  if (!all(is.na(basic.nms))) {
    x$basic <- data.frame(x[basic.nms])
    x <- x[-basic.nms]
  }
  if (!all(is.na(wiki.nms))) {
    x$thirdparty <- data.frame(x[wiki.nms])
    x <- x[-wiki.nms]
  }
  for (i in which(vapply(x[!names(x) %in% c("stats", "basic", "thirdparty")], class, character(1L)) == "list")) {
    x[i] <- b_format_tax_id_list(x[i], ids = ids)
  }
  x
}
b_format_tax_id_stats <- function(x) {
  if ("publicmarkersequences" %in% names(x)) {
    markers <- which(names(x) == "publicmarkersequences")
    data.frame(as.list(c(x[-markers], x[markers], recursive = TRUE)))
  } else {
    data.frame(as.list(c(x, recursive = TRUE)))
  }
}
b_format_tax_id_list <- function(x, ids) {
  lapply(
    b_nameself(names(x)),
    function(nm) {
      out <- c(x[[nm]], recursive = TRUE, use.names = TRUE)
      out <- data.table::data.table(taxid = ids, names(out), count = out)
      data.table::setnames(out, 2, nm)
      out
    }
  )
}
b_fill_empty <- function(x, types){
  len0 <- lengths(x) == 0
  if (any(len0)) {
    filler <- list(data.frame(col2rm = NA_character_))
    filler <- list(`names<-`(rep(filler, length(types)), types))
    x[which(len0)] <- filler
  }
  x
}
b_format_tax_id_output <- function(x, types, tree) {
  types <- b_nameself(types)
  #-- to match previous behavior of passing NAs
  x <- b_fill_empty(x, types)
  if (tree && length(types) == 1 &&
      !types %in% c("basic", "thirdparty", "images", "stats")) {
    x <- b_rbind(unlist(x, recursive = FALSE, use.names = T), idcol = "input")
    x$input <- as.integer(b_extract(x$input, "^[0-9]+(?=\\.)"))
  }  else {
    x <- lapply(types, b_grp_dataTypes, x = x, idcol = "input")
  }
  if (length(x) == 1) {
    x[[1]]
  } else {
    x
  }
}
b_grp_dataTypes_tree <- function(nm, x) {
  idcol <- if (nm == "stats" || nm == "thirdparty" || nm == "images") "taxid" else NULL
  out <- b_rbind(lapply(x, `[[`, nm), idcol = idcol)
  out$taxid <- as.integer(out$taxid)
  out
}
b_grp_dataTypes <- function(nm, x, idcol = FALSE) {
  out <- b_rbind(lapply(x, `[[`, nm), idcol = idcol)
  out[["input"]] <- as.integer(out[["input"]])
  out[names(out) == "col2rm"] <- NULL
  out
}






b_assert <- function(x,
                     what,
                     name = NULL,
                     check.length = NULL) {
  if (!length(name)) {
    name <- substitute(x)
  }
  msgLen <- if (length(check.length) && !isFALSE(check.length)) {
    b_assert_length(x = x, len = check.length, name = name,
                    stopOnFail = length(x) > 0)
  } else {
    NULL
  }
  msgClass <- if (length(x)) {
    b_assert_class(x = x, what = what, name = name,
                   is2nd = length(msgLen), stopOnFail = FALSE)
  } else {
    NULL
  }
  msg <- c(msgLen, msgClass)
  if (length(msg)) {
    stop(msg, call. = FALSE)
  }
}
b_assert_class <- function(x, what, name, is2nd = FALSE, stopOnFail = TRUE) {
  .fun <- if (stopOnFail) stop else paste0
  if (!inherits(x = x, what = what)) {
    if (!is2nd)
      .fun("'", name, "' must be of class ", b_ennum(what, "or"))
    else
      .fun(" and of class ", b_ennum(what, "or"))
  } else {
    NULL
  }
}
b_assert_length <- function(x, len, name, stopOnFail = TRUE) {
  len <- as.integer(len)
  if (!is.na(len) && len >= 0) {
    .fun <- if (stopOnFail) stop else paste0
    if (len == 0 && !length(x)) {
      .fun("'", name, "' can't be empty")
    } else if (len > 0 && length(x) != len) {
      .fun("'", name, "' must be length ", len)
    }
  }
}
b_assert_logical <- function(x, name = NULL) {
  b_assert_length(x, len = 1L, name = name)
  if (!length(name)) name <- substitute(x)
  x <- tolower(x)
  if (x == "true" || x == "1")
    TRUE
  else if (x == "false" || x == "0" || x == "na")
    FALSE
  else
    stop("'", name, "' should be one of TRUE or FALSE")
}
b_validate <- function(x, choices, name){
  wrong <- !x %in% choices
  if (any(wrong)) {
    stop(
      b_ennum(x[wrong], quote = TRUE),
      if (sum(wrong) > 1)
        " are not valid "
      else
        " is not a valid ",
      name,
      "\nChoices are ",
      b_ennum(choices, join_word = "or", quote = TRUE),
      call. = FALSE
    )
  }
}
b_get_db <- function(x){
  opts <- list(case_insensitive = TRUE)
  if (b_detect(x, '^COX[1I]$', opts_regex = opts))
    "COX1"
  else if (b_detect(x, '^pub(lic)?$|_public$', opts_regex = opts))
    "COX1_SPECIES_PUBLIC"
  else if (b_detect(x, '^spe(cies)?$|_species$', opts_regex = opts))
    "COX1_SPECIES"
  else if (b_detect(x, '^(cox[1I]_)?(l640)?bp$', opts_regex = opts))
    "COX1_L640bp"
  else
    x
}
b_assert_db <- function(x){
  b_assert(x, "character", name = "db", check.length = 1L)
  x <- b_get_db(x)
  b_validate(x, choices = b_db, name = "db")
  x
}
b_get_tax_division <- function(x){
  x <- tolower(x)
  x[b_detect(x, '^animal')] <- "Animalia"
  x[b_detect(x, '^prot')] <- "Protista"
  x[b_detect(x, '^fun')] <- "Fungi"
  x[b_detect(x, '^plant')] <- "Plantae"
  x
}
b_assert_tax_division <- function(x){
  if (length(x)) {
    b_assert(x, what = "character", name = "tax_division")
    x <- b_get_tax_division(x)
    b_validate(x, choices = b_tax_division, name = "tax_division")
  }
  x
}
b_get_tax_rank <- function(x){
  x <- tolower(x)
  x[b_detect(x, '^king')] <- "kingdom"
  x[b_detect(x, '^phy')] <- "phylum"
  x[b_detect(x, '^cla')] <- "class"
  x[b_detect(x, '^ord')] <- "order"
  x[b_detect(x, '^fam')] <- "family"
  x[b_detect(x, '^subfam')] <- "subfamily"
  x[b_detect(x, '^tribe')] <- "tribe"
  x[b_detect(x, '^gen')] <- "genus"
  x[b_detect(x, '^spe')] <- "species"
  x[b_detect(x, '^subspe')] <- "subspecies"
  x
}
b_assert_tax_rank <- function(x){
  if (length(x)) {
    b_assert(x, what = "character", name = "tax_rank")
    x <- b_get_tax_rank(x)
    b_validate(x, choices = b_tax_rank, name = "tax_rank")
  }
  x
}
b_assert_format <- function(x){
  b_assert(x, what = "character", check.length = 1L, name = "format")
  x <- tolower(x)
  if (x != "xml" && x != "tsv")
    stop("'format' should be one of 'xml' or 'tsv'")
  else
    x
}
b_get_dataTypes <- function(x){
  x <- tolower(x)
  if (any(x == "all")) {
    x <- "all"
  } else {
    # corrects for the json typo in case the option is taken from a previous query
    # and for short versions/typos
    x[x == "basics"] <- "basic"
    x[x == "depo" | x == "depositories"] <- "depository"
    x[x == "labs" | x == "sequencinglab"] <- "sequencinglabs"
    x[x == "stat"] <- "stats"
    x[x == "img"] <- "images"
    x[x == "wiki"] <- "thirdparty"
  }
  x
}
b_assert_dataTypes <- function(x){
  b_assert(x, what = "character", name = "dataTypes", check.length = 0L)
  x <- b_get_dataTypes(x)
  b_validate(x, choices = b_dataTypes, name = "dataTypes")
  x
}



# -- API requests helpers
b_url <- function(api) paste0('https://v4.boldsystems.org/index.php/', api)
b_pipe_params <- function(..., paramnames = ...names(), params = list(...)) {
  params <- b_rm_empty(params)
  if (!length(params))
    stop("You must provide a non-empty value to at least one of:\n\t",
         b_ennum(paramnames, join_word = "or", quote = TRUE), call. = FALSE)
  wt <- !vapply(params, is.character, logical(1))
  if (any(wt))
    stop(b_ennum(names(wt)[wt], quote = TRUE), " must be of class character", call. = FALSE)
  if (length(params$taxon)) {
    # in case it comes from `bold_tax_name()` (#84)
    params$taxon <- b_fix_taxonName(params$taxon)
  }
  vapply(params, paste, collapse = "|", character(1))
}
b_GET <- function(query, api, check = FALSE,
                  contentType = 'text/html; charset=utf-8', ...) {
  cli <- crul::HttpClient$new(url = b_url(api))
  res <- cli$get(query = query, ...)
  if (check) b_CHECK(res = res, contentType = contentType)
  else res
}
b_CHECK <- function(res, contentType) {
  w <- ""
  # get HTTP error if any
  if (res$status_code > 202) {
    x <- res$status_http()
    w <- paste0("HTTP ", x$status_code, ": ", x$message, "\n ", x$explanation)
  }
  # check if the content returned is of the right type
  if (res$response_headers$`content-type` != contentType) {
    w <- paste0(
      "Content was type '", res$headers$`content-type`,
      "' when it should've been type '", contentType, "'")
  }
  # if warning call it now
  if (nzchar(w)) warning(w, call. = FALSE, immediate. = TRUE)
  list(response = res, warning = w)
}
b_parse <- function(res, format, raise = TRUE, cleanData = FALSE, multiple = FALSE){
  if (raise) res$raise_for_status()
  res <- {
    if (!multiple)
      rawToChar(res$content)
    else
      paste0(rawToChar(res$content, multiple = TRUE), collapse = "")
  }
  res <- enc2utf8(res)
  if (res == "") {
    NA
  } else {
    if (b_detect(res, "Fatal error")) {
      # if returning partial output for bold_seq, might as well do that here too
      warning("the request timed out, see 'If a request times out'\n",
              "returning partial output")
      res <- b_drop(str = res, pattern = "Fatal error")
      # if missing, adding closing tag so it can be read properly
      if (format == "xml" && !b_detect(res, "</bold_records")) {
        res <- paste0(res, "</bold_records>")
      }
    }
    switch(
      format,
      xml = b_read_xml(res),
      json = b_read_json(res),
      tsv = b_read_tsv(res, cleanData = cleanData),
      fasta = b_read_fasta(res)
    )
  }
}
b_cleanData <- function(x, emptyValue = NA){
  col2clean <- vapply(x, function(x) {
    any(b_detect(x, "|", max_count = 1, fixed = TRUE), na.rm = TRUE)
  }, NA)
  col2clean <- which(col2clean, useNames = FALSE)
  for (.col in col2clean) {
    x[[.col]] <- b_replace(
      x[[.col]],
      # if the same text is repeated
      # or if only "||||"
      "^([^\\|]+)(\\|\\1)+$|^\\|+$",
      # keep first text/replace with nothing
      "$1")
  }
  x[x == ""] <- emptyValue
  x
}
b_read_json <- function(x){
  jsonlite::parse_json(x, simplifyVector = TRUE, flatten = TRUE)
}
b_read_xml <- function(x) {
  # DON'T REMOVE OPTIONS
  # necessary for large request!
  xml2::read_xml(x, options = c("NOBLANKS", "HUGE"))
}
b_read_tsv <- function(x, header = TRUE, sep = "\t",
                       cleanData = FALSE, ...) {
  # -- fix for issue 104 ----------------------------------------
  # wild '\n' characters in some fields
  if (stringi::stri_detect_regex(x, "[^\r]\n[^\r]")) {
    x_names <- stringi::stri_extract_first_regex(x, "^[^\r\n]+")
    x_names <- stringi::stri_split_regex(x_names, "\t", simplify = TRUE)
    pttrn <- paste0("(?<!", x_names[length(x_names)], "|\r)\n(?!=\r)")
    x <- stringi::stri_replace_all_regex(x, pttrn, " ")   
  }
  # -------------------------------------------------------------
  x <- data.table::setDF(
    data.table::fread(
      text = x,
      header = header,
      sep = sep,
      ...
    )
  )
  if (cleanData) {
    b_cleanData(x)
  } else {
    x
  }
}
b_nameself <- function(x){
  `names<-`(x, x)
}
b_rm_empty <- function(x) {
  if (!length(x)) {
    NULL
  } else {
    x[lengths(x) > 0 & nzchar(x)]
  }
}
b_rbind <- function(x, fill = TRUE, use.names = TRUE, idcol = NULL) {
  (x <- data.table::setDF(
    data.table::rbindlist(l = x, fill = fill, use.names = use.names, idcol = idcol))
  )
}
b_ennum <- function(x, join_word = "and", quote = FALSE) {
  if (!is.character(x)) x <- as.character(x)
  x <- x[nzchar(x)]
  if (quote) {
    x <- paste0("'", x, "'")
  }
  n <- length(x)
  if (n > 1)
    paste(paste(x[-n], collapse = ", "), join_word, x[n])
  else
    x
}
b_fix_taxonName <- function(x){
  # see issue #84
  # check if supposed to:
  x <- b_replace(x,
                 # be quoted; keep quoted text
                 " ('[^']*)$",
                 # add closing quote
                 " \\\\$1\\\\'")
  x <- b_replace(x,
                 # be in parenthesis; keep parenthesis text
                 " (\\([^\\(]*)$",
                 # add closing parenthesis
                 " $1)")
  x <- b_replace(x,
                 # end with a dot (there might be more cases, but haven't seen them yet)
                 "( sp(\\. nov)?$)",
                 # add end dot
                 "$1.")
  x
}


b_extract <- function(str, pattern, mode = "all",
                      fixed = FALSE, simplify = TRUE, ...) {
  switch(mode,
         first = {
           if (!fixed)
             stringi::stri_extract_first_regex(str = str, pattern = pattern, ...)
           else
             stringi::stri_extract_first_fixed(str = str, pattern = pattern, ...)
         },
         last = {
           if (!fixed)
             stringi::stri_extract_last_regex(str = str, pattern = pattern, ...)
           else
             stringi::stri_extract_last_fixed(str = str, pattern = pattern, ...)
         },
         all = {
           if (!fixed)
             stringi::stri_extract_all_regex(str = str,
                                             pattern = pattern,
                                             simplify = simplify,
                                             ...)
           else
             stringi::stri_extract_all_fixed(str = str,
                                             pattern = pattern,
                                             simplify = simplify,
                                             ...)
         },
         str)
}
b_words <- function(str, mode = "first", ...) {
  switch(mode,
         first = stringi::stri_extract_first_words(str = str, ...),
         last = stringi::stri_extract_last_words(str = str, ...),
         all = stringi::stri_extract_all_words(str = str, ...),
         str
  )
}
b_replace <- function(str, pattern, replacement, fixed = FALSE, ...) {
  if (!fixed)
    stringi::stri_replace_all_regex(str = str, pattern = pattern,
                                    replacement = replacement, ...)
  else
    stringi::stri_replace_all_fixed(str = str, pattern = pattern,
                                    replacement = replacement, ...)
}
b_detect <- function(str, pattern, fixed = FALSE, ...) {
  if (!fixed)
    stringi::stri_detect_regex(str = str, pattern = pattern, ...)
  else
    stringi::stri_detect_fixed(str = str, pattern = pattern, ...)
}
b_split <- function(str, pattern, fixed = FALSE, ...) {
  if (!fixed)
    stringi::stri_split_regex(str = str, pattern = pattern, ...)
  else
    stringi::stri_split_fixed(str = str, pattern = pattern, ...)
}
b_count <- function(str, pattern, fixed = FALSE, ...) {
  if (!fixed)
    stringi::stri_count_regex(str = str, pattern = pattern, ...)
  else
    stringi::stri_count_fixed(str = str, pattern = pattern, ...)
}
b_drop <- function(str, pattern) {
  to <- stringi::stri_locate_first_fixed(str = str, pattern = pattern)[[1]] - 1L
  stringi::stri_sub(str = str, to = to)
}
b_lines <- function(str) {
  stringi::stri_split_lines1(str = str)
}

b_read_fasta <- function(x){
  x <- b_lines(str = x)
  if (length(x) %% 2 && # if length(x) %% 2 not 0, it's TRUE
      b_detect(x[length(x)], "^\\s*$")) {
    x <- x[-length(x)]
  }
  id_line <- b_detect(x,"^>")
  n <- which(id_line)
  id <- b_split(str = x[id_line], pattern = ">|\\|", omit_empty = TRUE, simplify = NA, n = 4)
  id <- `names<-`(as.data.frame(id), c("processid", "identification", "marker", "accession"))
  if (all(diff(c(n, length(x) + 1L)) == 2)) {
    sequence <- x[n + 1L]
    data.frame(id, sequence)
  } else if (sum(id_line) == sum(!id_line)) {
    # shouldn't happen but who knows
    warning("The file had an even number of ids and sequences, but they weren't in the proper order.",
            "\n  This shouldn't happen. Output may contain errors.",
            "\n  Please open an issue so we can see when this happens.")
    sequence <- x[!id_line]
    data.frame(id, sequence)
  } else {
    warning("The file had an uneven number of ids and sequenceuences.",
            "\n  This shouldn't happen. Returning data as a list of lines.",
            "\n  Please open an issue so we can see when this happens.")
    x
  }
}

bold_tax_name <- function(name, fuzzy = FALSE, response = FALSE,
                          tax_division = NULL, tax_rank = NULL, ...) {
  if (missing(name)) stop("argument 'name' is missing, with no default")
  b_assert(name, "character", check.length = 0L)
  # fix for #84:
  name <- b_replace(name, "(?<=[^\\\\])'", "\\\\'")
  if (!missing(response)) response <- b_assert_logical(response)
  if (!missing(fuzzy)) fuzzy <- b_assert_logical(fuzzy)
  if (!missing(tax_division)) tax_division <- b_assert_tax_division(tax_division)
  if (!missing(tax_rank)) tax_rank <- b_assert_tax_rank(tax_rank)
  res <- lapply(b_nameself(name), function(x)
    b_GET(
      query = c(taxName = x, fuzzy = tolower(fuzzy)),
      api = "API_Tax/TaxonSearch",
      check = TRUE,
      ...
    ))
  if (response) {
    res
  } else {
    out <- b_rbind(lapply(res, b_process_tax_name,
                          tax_division = tax_division,
                          tax_rank = tax_rank),
                   idcol = "input")
    w <- lapply(res, `[[`, "warning")
    # using %in% to avoid NAs
    noMatchDiv <- out$taxid %in% -1L
    noMatchRank <- out$taxid %in% -2L
    noMatch <- out$taxid %in% -3L
    out$taxid[noMatchDiv | noMatchRank | noMatch] <- NA_integer_
    w[noMatchDiv] <- "Request returned no match with supplied 'tax_division'"
    w[noMatchRank] <- "Request returned no match with supplied 'tax_rank'"
    w[noMatch] <- "Request returned no match"
    attr(out, "errors") <- b_rm_empty(w)
    attr(out, "params") <- list(fuzzy = fuzzy,
                                tax_division = tax_division,
                                tax_rank = tax_rank)
    out
  }
}

b_process_tax_name <- function(x, tax_division, tax_rank) {
  if (!nzchar(x$warning)) {
    out <- b_parse(x$response, format = "json")
    if (length(out) && length(out$top_matched_names)) {
      out <- data.frame(out$top_matched_names)
      out.nms <- names(out)
      # see issue #84
      if (any(out.nms == "taxon")) out$taxon <- b_fix_taxonName(out$taxon)
      if (any(out.nms == "parentname")) out$parentname <- b_fix_taxonName(out$parentname)
      if (any(out.nms == "taxonrep")) out$taxonrep <- b_fix_taxonName(out$taxonrep)
      if (length(tax_division) && any(out.nms == "tax_division")) {
        if (sum(out$tax_division %in% tax_division)) {
          out <- out[out$tax_division %in% tax_division,]
        } else {
          out <- data.frame(taxid = -1L) # request returned no match with supplied tax_division
        }
      }
      if (length(tax_rank)  && any(out.nms == "tax_rank")) {
        if (sum(out$tax_rank %in% tax_rank)) {
          out <- out[out$tax_rank %in% tax_rank,]
        } else {
          out <- data.frame(taxid = -2L) # request returned no match with supplied tax_rank
        }
      }
    } else {
      out <- data.frame(taxid = -3L) # request returned no match
    }
  } else {
    out <- data.frame(taxid = NA_integer_) # resquest had an error
  }
  out
}

b_dataTypes <- c(
  "basic",
  "stats",
  "geo",
  "images",
  "sequencinglabs",
  "depository",
  "thirdparty",
  "all"
)
b_tax_division <- c("Animalia", "Protista", "Fungi", "Plantae")
b_tax_rank <-
  c(
    "kingdom",
    "phylum",
    "class",
    "order",
    "family",
    "subfamily",
    "tribe",
    "genus",
    "species",
    "subspecies"
  )
b_db <- c('COX1', 'COX1_SPECIES', 'COX1_SPECIES_PUBLIC', 'COX1_L640bp')