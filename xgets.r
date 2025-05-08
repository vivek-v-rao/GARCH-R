# silence package startup messages
suppressMessages(suppressWarnings(library(gets)))
suppressMessages(suppressWarnings(library(xts)))

# Read CSV and convert to xts
df     <- read.csv("spy_tlt.csv", stringsAsFactors = FALSE)
dates  <- as.Date(df[[1]])
prices <- xts(df[, 2:ncol(df)], order.by = dates)
narch  <- 5

# Loop over each series and fit ARCH(1:5)
for (col_name in colnames(prices)) {
  cat("\n--- Fitting ARCH(", narch, ") for ", col_name, " using gets ---\n", sep = "")
  
  # compute log-returns, drop initial NA
  ret_xts <- na.omit(diff(log(prices[, col_name])))
  y       <- as.numeric(ret_xts)
  
  # estimate AR(0) + ARCH(1:5) in log-variance
  fit <- suppressWarnings(
    arx(y, arch = 1:narch)
  )
  
  # print the mean/intercept and ARCH coefficients
  cat("\nMean equation coefficients:\n")
  print(fit$mean.results)
  
  cat("\nARCH coefficients:\n")
  print(fit$variance.results)
}
