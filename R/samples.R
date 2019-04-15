#' Read the sample experiment table
#'
#' This function read the sample experiment table
#' and checks that all the mzML and mzid files exist.
#' The names of the files (without extensions) must
#' be defined in a column `"name"` in the sample experiment
#' table.
#'
#' @param f `character(1)` defining the name of the
#' sample experiment table in comma separated values
#' (csv) format. Default is `"SampleExperimentTable.csv"`.
#' @param mzml `character(1)` defining the directory name
#' where the mzML files can be found. Default is
#' `"mzml"`.
#' @param mzid `character(1)` defining the directory name
#' where the mzid files can be found. Default is
#' `"msgf"`.
#'
#' @return `data.frame` with the sample experiment table.
#' @export
#' @md
#' @importFrom utils read.csv
readSampleExperimentTable <- function(f = "SampleExperimentTable.csv",
                                      mzml = "mzml",
                                      mzid = "msgf") {
  exp <- read.csv(f, stringsAsFactors = FALSE)
  wdir <- dirname(f)
  ## drop constant variables
  exp <- dropConstantVariables(exp)
  ## check that mzml files exist
  mzml_files <- file.path(wdir, mzml,
                          paste(exp$name, "mzML", sep = "."))
  mzml_exist <- file.exists(mzml_files)
  if (!all(mzml_exist))
    stop(sum(!mzml_exist), " mzML files don't exist.")
  ## check that mzid files exist
  mzid_files <- file.path(wdir, mzid,
                          paste(exp$name, "mzid", sep = "."))
  mzid_exist <- file.exists(mzid_files)
  if (!all(mzid_exist))
    stop(sum(!mzid_exist), " mzid files don't exist.")
  exp
}

#' Print a experiment summary
#'
#' @param exp An experiment summary, as produced by `readSampleExperimentTable`.
#' @return Used for its side effect.
#' @export
#' @md
experimentSummary <- function(exp) {
  cat("Number of sample:", nrow(exp), "\n")
  cat("With variable:\n")
  for (i in seq_len(ncol(exp)))
    cat(paste0(" - ", names(exp)[i], ": ", length(unique(exp[, i])), "\n"))
}

#' Print a summary hierarchy.
#'
#' @param exp An experiment summary, as produced by `readSampleExperimentTable`.
#' @param fcol Variable names to be used for the hierarchy/
#' @param orderBy Optional variable to order.
#' @md
#' @return Used for printing only.
#' @export
experimentHierarchy <- function(exp, fcol,
                                orderBy = NULL) {
  x <- unique(exp[, fcol])
  stopifnot(orderBy %in% fcol)
  if (!is.null(orderBy))
    x <- x[order(x[, orderBy]), ]
  x
}


#' Drop constant variable
#'
#' @param exp An experiment summary, as produced by `readSampleExperimentTable`.
#' @return An update experiment table without any constant variables.
#' @export
#' @md
dropConstantVariables <- function(exp) {
  nvar <- apply(exp, 2, function(x) length(unique(x)))
  if (any(nvar <= 1))
    message("Dropping constant variable(s): ",
            paste(names(exp)[nvar <= 1], collapse = ", "))
  exp[, nvar > 1]
}
