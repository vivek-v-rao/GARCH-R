# fit GARCH models using the tseries package
suppressMessages(suppressWarnings(library(tseries)))
suppressMessages(suppressWarnings(library(xts)))

# Read CSV and turn into an xts price series
prices_file <- "spy_tlt.csv"
df     <- read.csv(prices_file, stringsAsFactors = FALSE)
dates  <- as.Date(df[[1]])      
prices <- xts(df[, 2:ncol(df)], order.by = dates)

cat("Data file:", prices_file, "\n")
cat("Date range:", format(start(prices)), "to", format(end(prices)), "\n")
cat("Number of observations:", nrow(prices), "\n")

# Loop over each column
for (col_name in colnames(prices)) {
  cat("\n--- Fitting GARCH(1,1) for", col_name, "---\n")
  
  # compute log‐returns and drop the initial NA
  ret_xts <- na.omit(diff(log(prices[, col_name])))
  
  # fit a GARCH(1,1) via tseries::garch (Gaussian innovations)
  fit <- suppressWarnings(
    garch(as.numeric(ret_xts), order = c(1, 1), trace = FALSE)
  )
  
  # print the fitted model summary
  print(summary(fit))
}
