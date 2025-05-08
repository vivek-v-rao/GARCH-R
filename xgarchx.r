# fit GARCH models using the garchx package
suppressMessages(suppressWarnings(library(garchx)))
suppressMessages(suppressWarnings(library(xts)))

# Read CSV and convert to an xts price series
df     <- read.csv("spy_tlt.csv", stringsAsFactors = FALSE)
dates  <- as.Date(df[[1]])                   
prices <- xts(df[, 2:ncol(df)], order.by = dates)

# Loop over each column and fit a GARCH(1,1) model with robust SEs
for (col_name in colnames(prices)) {
  cat("\n--- Fitting GARCH(1,1) for", col_name, "using garchx (robust SE) ---\n")
  
  # compute log-returns and drop the initial NA
  ret_xts <- na.omit(diff(log(prices[, col_name])))
  y       <- coredata(ret_xts)
  
  # fit via Quasi‑ML, request robust covariance
  fit <- suppressWarnings(
    garchx(y, order = c(1, 1), vcov.type = "robust")
  )
  
  # print the built‑in coefficient table
  print(fit)
  
  # extract numeric estimates and SEs
  coefs <- coef(fit)               # intercept, arch1, garch1
  ses   <- sqrt(diag(vcov(fit)))   # robust SEs :contentReference[oaicite:0]{index=0}
  
  cat("\nParameter estimates:\n")
  print(coefs)
  
  cat("\nRobust standard errors:\n")
  print(ses)
}
