# fit GARCH models using tsgarch
suppressMessages(suppressWarnings(library(tsgarch)))
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
  
  # specify the model
  spec <- garch_modelspec(
    y            = ret_xts,
    constant     = TRUE,
    order        = c(1, 1),
    distribution = "std"
  )
  
  # fit via maximum likelihood + autodiff, suppress any warnings
  fit <- suppressWarnings(estimate(spec, solver = "nloptr"))
  print(summary(fit))
}
