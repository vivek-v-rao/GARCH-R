# fit log-GARCH models using the lgarch package
suppressMessages(suppressWarnings(library(lgarch)))
suppressMessages(suppressWarnings(library(xts)))

# Read CSV and convert to an xts price series
df     <- read.csv("spy_tlt.csv", stringsAsFactors = FALSE)
dates  <- as.Date(df[[1]])                   
prices <- xts(df[, 2:ncol(df)], order.by = dates)

# Loop over each column and fit a log-GARCH(1,1) model
for (col_name in colnames(prices)) {
  cat("\n--- Fitting log-GARCH(1,1) for", col_name, "---\n")
  
  # compute log-returns and drop the initial NA
  ret_xts <- na.omit(diff(log(prices[, col_name])))
  y       <- coredata(ret_xts)  # plain numeric vector
  
  # fit via quasi-ML, suppress warnings
  fit <- suppressWarnings(
    lgarch(y, arch = 1, garch = 1, method = "ml")
  )
  
  # print the lgarch summary (which shows intercept, alpha1, beta1)
  cat("\nModel output:\n")
  print(fit)  
  
  # extract and print parameter vector
  cat("\nParameter estimates:\n")
  print(coef(fit))               # coef.lgarch method :contentReference[oaicite:0]{index=0}

  # compute and print standard errors
  cat("\nStandard errors:\n")
  se <- sqrt(diag(vcov(fit)))    # vcov.lgarch method :contentReference[oaicite:1]{index=1}
  print(se)
}
