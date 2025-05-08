# fit GARCH models using the tseries package
suppressMessages(suppressWarnings(library(tseries)))
suppressMessages(suppressWarnings(library(xts)))

# Read CSV and turn into an xts price series
df     <- read.csv("spy_tlt.csv", stringsAsFactors = FALSE)
dates  <- as.Date(df[[1]])      
prices <- xts(df[, 2:ncol(df)], order.by = dates)

# Loop over each column
for (col_name in colnames(prices)) {
  cat("\n--- Fitting GARCH(1,1) for", col_name, "---\n")
  
  # compute log‐returns and drop the initial NA
  ret_xts <- na.omit(diff(log(prices[, col_name])))
  
  # fit an sGARCH(1,1) via tseries::garch (Gaussian innovations)
  fit <- suppressWarnings(
    garch(as.numeric(ret_xts), order = c(1, 1), trace = FALSE)
  )
  
  # print the fitted model summary
  print(summary(fit))
}
