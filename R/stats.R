#' @importFrom msmsTests msms.edgeR
#' @importFrom stats p.adjust
msms_edgeR_test <- function(e,
                            null.f,
                            alt.f,
                            fnm,
                            test_name) {
  div <- apply(exprs(e), 2, sum)
  res <- msms.edgeR(e, alt.f, null.f,
                    div = div, fnm = fnm)
  res$adj.p.values <- p.adjust(res$p.value, method = "BH")
  if (!missing(test_name))
    names(res) <- paste(names(res), test_name, sep = "_")
  fData(e) <- cbind(fData(e), res)
  return(e)
}
