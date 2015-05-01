get_uid2("Echinacea")
# division
## Echinacea
get_uid2(sciname = "Echinacea", division = "eudicots")
get_uid2(sciname = "Echinacea", division = "sea urchins")
## Satyrium
get_uid2(sciname = "Satyrium")
get_uid2(sciname = "Satyrium", division = "monocots")
get_uid2(sciname = "Satyrium", division = "butterflies")
# rank
get_uid2(sciname = "Pinus", rank = "genus")
get_uid2(sciname = "Pinus", rank = "subgenus")

#
get_uid2 <- function(sciname, ask = TRUE, verbose = TRUE, rows = NA, division = NULL, rank = NULL) {

  fun <- function(sciname, ask, verbose, rows) {
    mssg(verbose, "\nRetrieving data for taxon '", sciname, "'\n")
    sciname <- gsub(" ", "+", sciname)
    searchurl <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=taxonomy&term=",
                       sciname, sep = "")
    errors_to_catch <- c("Could not resolve host: eutils.ncbi.nlm.nih.gov")
    xml_result <- xmlParse(repeat_until_it_works(getURL,
                                                 catch = errors_to_catch,
                                                 url = searchurl))
    # NCBI limits requests to three per second
    Sys.sleep(0.33)
    uid <- xpathSApply(xml_result, "//IdList/Id", xmlValue)
    if (length(uid) == 0) { # if taxon name is not found
      uid <- NA
    } else {
      uid <- sub_vector(uid, rows)
    }
    att <- 'found'
    # not found on ncbi
    if (length(uid) == 0) {
      mssg(verbose, "Not found. Consider checking the spelling or alternate classification")
      uid <- NA
      att <- 'not found'
    }
    # more than one found on ncbi -> user input
    if (length(uid) > 1) {
      if (ask) {
        baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=taxonomy"
        ID <- paste("ID=", paste(uid, collapse = ","), sep = "")
        searchurl <- paste(baseurl, ID, sep = "&")
        #         tt <- getURL(searchurl)
        errors_to_catch <- c("Could not resolve host: eutils.ncbi.nlm.nih.gov")
        tt <- repeat_until_it_works(getURL,
                                    catch = errors_to_catch,
                                    url = searchurl)
        ttp <- xmlTreeParse(tt, useInternalNodes = TRUE)
        df <- ldply(xmlToList(ttp), data.frame, stringsAsFactors = FALSE)
        df <- df[df$Item..attrs != 'String', c(2, 5, 7)]
        names(df) <- c("uid", "rank", "division")
        rownames(df) <- 1:nrow(df)

        if (!is.null(division) || !is.null(rank)) {
          if (!is.null(division)) {
            if (division %in% df$division) {
              df <- df[which(df$division %in% division), ]
            }
          }
          if (!is.null(rank)) {
            if (rank %in% df$rank) {
              df <- df[which(df$rank %in% rank), ]
            }
          }
          uid <- df$uid
        } else {
          # prompt
          message("\n\n")
          message("\nMore than one UID found for taxon '", sciname, "'!\n
                Enter rownumber of taxon (other inputs will return 'NA'):\n")
          print(df)
          take <- scan(n = 1, quiet = TRUE, what = 'raw')

          if (length(take) == 0) {
            take <- 'notake'
            att <- 'nothing chosen'
          }
          if (take %in% seq_len(nrow(df))) {
            take <- as.numeric(take)
            message("Input accepted, took UID '", as.character(df$uid[take]), "'.\n")
            uid <- as.character(df$uid[take])
            att <- 'found'
          } else {
            uid <- NA
            att <- 'not found'
            mssg(verbose, "\nReturned 'NA'!\n\n")
          }
        }
      } else {
        uid <- NA
        att <- 'NA due to ask=FALSE'
      }
    }
    return(data.frame(uid, att, stringsAsFactors = FALSE))
  }
  sciname <- as.character(sciname)
  outd <- ldply(sciname, fun, ask, verbose, rows)
  out <- structure(outd$uid, class = "uid", match = outd$att)
  add_uri(out, 'http://www.ncbi.nlm.nih.gov/taxonomy/%s')
}
