suppressMessages(suppressWarnings(library(fGarch)))

# Read data
x = read.csv("spy_tlt.csv")
prices = x[, 2:ncol(x)]  # Select all columns from 2 onward

# Loop over each column in prices
for (col in 1:ncol(prices)) {
  # Get column name and data
  col_name = colnames(prices)[col]
  price_series = prices[[col]]
  
  # Compute log returns
  xret = diff(log(price_series))
  
  # Fit GARCH(1,1) model with std distribution
  cat("\n--- Fitting GARCH(1,1) for", col_name, "---\n")
  fit = garchFit(~garch(1,1), cond.dist="std", data=xret, trace=FALSE)
  print(fit)
}
