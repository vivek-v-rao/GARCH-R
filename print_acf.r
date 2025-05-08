acf_labels <- function(nacf) {
    # return labels "ACF_1","ACF_2",...,"ACF_nacf"
    if (nacf > 0) {
        xx <-  vector(mode="character",nacf)
        for (i in 1:nacf) xx[[i]] = sprintf("ACF_%.0f",i)
    } else {
        xx <- ""[-1]
    }
    return(xx)
}

print_acf <- function(xx,nacf=5,header="") {
    if (nacf < 1) return()
    cat(header)
    symbols <- names(xx)
    nvar <- ncol(xx)
    cat("AUTOCORRELATIONS:")
    cat("\n  symbol",sprintf("%6s",acf_labels(nacf)),"\n")
    for (icol in 1:nvar) {
        cat(sprintf("%8s",symbols[[icol]]),sprintf("%6.3f",acf(xx[,icol],lag.max=nacf)$acf[2:(nacf+1)]),"\n")
    }
}
