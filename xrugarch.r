# silence package startup messages
suppressMessages(suppressWarnings(library(rugarch)))
suppressMessages(suppressWarnings(library(xts)))

# Read CSV and turn into an xts price series
df     <- read.csv("spy_tlt.csv", stringsAsFactors = FALSE)
dates  <- as.Date(df[[1]])      
prices <- xts(df[, 2:ncol(df)], order.by = dates)

# Loop over each column
for (col_name in colnames(prices)) {
  cat("\n--- Fitting GARCH(1,1) for", col_name, "---\n")
  
  # compute log‐returns and drop the initial NA
  price_series <- prices[, col_name]
  ret_xts      <- na.omit(diff(log(price_series)))
  
  # specify a standard GARCH(1,1) with Student‑t errors
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std"
  )
  
  # fit the model, suppressing any warnings
  fit <- suppressWarnings(
    ugarchfit(spec = spec, data = ret_xts, solver = "nloptr")
  )
  
  # print the fitted model summary
  show(fit)
}
