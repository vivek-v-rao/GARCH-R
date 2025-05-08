print_stats_ret <- function(xret, nacf = 5, obs.year = 252) {
    nsym    <- ncol(xret)
    symbols <- names(xret)
    cat("\n         symbol",
        sprintf("%8s", c("mean","sd","Sharpe","skew","kurtosis","min","max", acf_labels(nacf))),
        "\n")
    for (isym in 1:nsym) {
        zret   <- xret[, isym]
        xmean  <- mean(zret)
        xrms   <- sqrt(sum(zret^2) / length(zret))
        xacf   <- 0[-1]
        if (nacf > 0) xacf <- acf(zret, lag.max = nacf)$acf[2:(nacf+1)]
        cat(
          sprintf("%15s", symbols[[isym]]),
          sprintf("%8.3f", c(
            xmean,
            xrms,
            sqrt(obs.year)*xmean/xrms,
            skewness(zret),
            kurtosis(zret),
            min(zret),
            max(zret),
            xacf
          )),
          "\n"
        )
    }
}