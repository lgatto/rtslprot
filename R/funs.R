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
