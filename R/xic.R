##' Produces a MS2 total ion count XIC
##'
##' @title
##' @param object An object of class OnDiskMSnExp
##' @param rtr numeric(2) with the retention time range
##' @param mzr numeric(2) with the M/Z range
##' @return Invisibly returns a data.frame with selected spectra names,
##'     retention times and MS2 total ion counts.
##' @examples
##' \dontrun{
##' mzr <- c(968.9491, 968.9691) ## 968.9591 +/- 0.01
##' rtr <- c(6800, 7000)
##' res <- plotxic(x, rtr, mzr) ## produces the plot
##' res ## contains the data used to plot
##' }
plotxic <- function(object, rtr, mzr) {
    rtsel <- rtime(object) > rtr[1] & rtime(object) < rtr[2]
    mzsel <- precursorMz(object) > mzr[1] & precursorMz(object) < mzr[2]
    sel <- rtsel & mzsel
    res <- data.frame(fn = featureNames(object)[sel],
                      rt = rtime(object)[sel],
                      tic = tic(object)[sel],
                      row.names = NULL)
    plot(res$rt, res$tic, type = 'b')
    invisible(res)
}