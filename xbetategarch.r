# silence package startup messages
suppressMessages(suppressWarnings(library(betategarch)))
suppressMessages(suppressWarnings(library(xts)))

# Read CSV and convert to an xts price series
df     <- read.csv("spy_tlt.csv", stringsAsFactors = FALSE)
dates  <- as.Date(df[[1]])                   # assume first column is YYYY-MM-DD
prices <- xts(df[, 2:ncol(df)], order.by = dates)

# Loop over each column and fit a 1st-order Beta-Skew-t-EGARCH model
for (col_name in colnames(prices)) {
  cat("\n--- Fitting Beta-Skew-t-EGARCH for", col_name, "---\n")
  
  # compute log-returns and drop the initial NA
  ret_xts <- na.omit(diff(log(prices[, col_name])))
  
  # extract as a plain numeric vector (tegarch() expects a numeric vector)
  y <- coredata(ret_xts)
  
  # fit via maximum likelihood, suppressing any warnings
  fit <- suppressWarnings(
    tegarch(y)                     # default is 1st-order Beta-Skew-t-EGARCH
  )
  
  # print a model summary
  print(summary(fit))
}
