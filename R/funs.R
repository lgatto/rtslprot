## rtslprot functions
## meant for local use and fast testing without the need of changing the package
make_psm_MSnSet <- function(mzid_files,
                            fdr, level) {
  ## Create MSnID
  msnid <- MSnID()
  msnid <- read_mzIDs(msnid, mzid_files)
  ## TODO - filter MSnID - see vignette
  msnid <- apply_filter(msnid, "isDecoy == FALSE")
  ## export to MSnSet
  td <- as(msnid, "data.table")
  td$count <- 1
  readMSnSet2(td, ecol = ncol(td))
}

make_pep_MSnSet <- function(mzid_files,
                            fdr, level,
                            fcol = "pepSeq") {
  e <- make_psm_MSnSet(mzid_files, fdr, level)
  combineFeatures(e, fcol = fcol, fun = sum)
}

##' A function to convert and MSnID to an MSnSet. Used to replacet the MSnID
##' function `as(., "MSnSet")` that drops features. Eventually, this function
##' will replace the one in MSnID.
##'
##' @title Convert and MSnID to an MSnSet
##' @param x An object of class `MSnID`.
##' @param fcol Feature variable used to combine PSMs.
##' @return An `MSnSet` object.
##' @md
##' @export
##' @import MSnID
as_MSnSet <- function(x, fcol = NULL) {
    stopifnot(inherits(x, "MSnID"))
    td <- as(x, "data.table")
    td$e <- 1
    ## Create a PSM-level MSnSet
    x <- readMSnSet2(td, ecol = which(colnames(td) == "e"))
    if (!is.null(fcol)) {
        ## If there's an fcol, combine at that level by summing PSM counts
        stopifnot(fcol %in% fvarLabels(x))
        x <- combineFeatures(x, fcol = fcol, method = sum)
    }
    return(x)
}
