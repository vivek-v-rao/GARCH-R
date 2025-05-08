# Reads CSV price data (dates as rownames), converts to xts,
# computes scaled daily returns, reports summary statistics (mean, sd,
# Sharpe ratio, skewness, kurtosis, min, max, acf), and fits
# GARCH models via the rugarch package
.proctime00 = proc.time()
options("getSymbols.warning4.0" = FALSE)
options(width = 1000)
sink("nul")
source("print_acf.r")
source("print_stats.r")
suppressMessages(suppressWarnings({
    library(rugarch)
    library(PerformanceAnalytics)  # imports Return.calculate
    library(moments)         # imports kurtosis
}))
sink()

print_rugarch <- function(zret,
                          garch.spec    = ugarchspec(),
                          symbol        = "",
                          print.inf.crit = FALSE,
                          print.garch.ci = FALSE,
                          garch.verbose  = FALSE) {
    garch.fit <- ugarchfit(garch.spec, zret)
    cat("\nGARCH model for", symbol, "\n")
    print(coef(garch.fit))
    if (print.garch.ci)  print(confint(garch.fit))
    if (print.inf.crit)  print(infocriteria(garch.fit))
    if (garch.verbose)   print(garch.fit)
    return(garch.fit)
}

# ── parameters ───────────────────────────────────────────────────────────────
scale.vol              = FALSE
target.vol             = 10.0
do.garch               = TRUE
garch.verbose          = FALSE
print.garch.ci         = FALSE
print.inf.crit         = TRUE
print.ret.stats        = TRUE
print.corr             = TRUE
scale.ret              = 100
nacf                   = 0
ar.order.garch         = 0
ma.order.garch         = 0
archm                  = TRUE
garch.model            = "fGARCH"
garch.submodel         = "GJRGARCH" # "GARCH" 
distribution.model     = "std" # "norm" 
demean.ret.obs         = FALSE
prices.file            = "spy_tlt.csv"
max.sym                = 1000

# ── load prices ──────────────────────────────────────────────────────────────
cat("\nprices file =", prices.file, "\n")
prices <- read.csv(prices.file,
                   header           = TRUE,
                   stringsAsFactors = FALSE,
                   row.names        = 1)

# ── convert to xts to avoid xtsAttributes<- errors ───────────────────────────
library(xts)
prices <- as.xts(prices)

if (max.sym < ncol(prices)) {
    prices <- prices[, 1:max.sym]
}
prices <- na.omit(prices)

symbols <- names(prices)
periods <- "days"
cat("\nperiod =", periods, "\n")

if (periods %in% c("weeks","months","quarters","years")) {
    prices <- prices[endpoints(prices, periods)]
}

nobs <- nrow(prices)
cat("\nnobs =", nobs, "\n")

# ── compute returns ──────────────────────────────────────────────────────────
xret <- as.data.frame(scale.ret * na.omit(Return.calculate(prices)))

# ── optional demeaning & adding means ────────────────────────────────────────
if (demean.ret.obs) {
    if (nacf > 0) print_acf(xret, nacf = nacf, header = "\n")
    cat("\ndemeaning returns by observation\n")
    xret <- xret - rowMeans(xret)
}

nret <- nrow(xret)
nsym <- ncol(xret)
cat("\n#returns =", nret, "\n#symbols =", nsym, "\n")

# ── optional volatility scaling ─────────────────────────────────────────────
if (scale.vol) {
    cat("\nreturns scaled by volatility\n")
    for (i in 1:nsym) {
        xret[, i] <- target.vol * xret[, i] / sd(xret[, i])
    }
}

# ── optional descriptive stats & correlation ────────────────────────────────
options(digits = 3)
if (print.corr && nsym > 1) {
    cat("\n CORRELATIONS\n")
    print(round(cor(xret), 3))
}

# ── fit GARCH if requested ──────────────────────────────────────────────────
fmt_g <- "\n%15s:"
if (do.garch) {
    garch.spec <- ugarchspec(
        variance.model    = list(model = garch.model, submodel = garch.submodel),
        mean.model        = list(archm = archm, armaOrder = c(ar.order.garch, ma.order.garch)),
        distribution.model = distribution.model
    )
    cat(sprintf(fmt_g, "garch.model"),    garch.model)
    cat(sprintf(fmt_g, "garch.submodel"), garch.submodel)
    cat(sprintf(fmt_g, "distribution"),    distribution.model)
    cat("\n")
}

if (print.ret.stats) print_stats_ret(xret, nacf)

if (do.garch) {
    for (isym in 1:nsym) {
        print_rugarch(
            xret[, isym],
            garch.spec,
            symbol        = symbols[[isym]],
            print.inf.crit = print.inf.crit,
            print.garch.ci = print.garch.ci,
            garch.verbose  = garch.verbose
        )
    }
}
if (nacf > 0) {
    print_acf(xret, nacf = nacf, header = "\n")
}

cat("\nfinished xxfit_rugarch.r (2)\n")
cat("\nfor #symbols =", length(symbols),
    "\nTime elapsed(s):", (proc.time() - .proctime00)[3], "\n")
