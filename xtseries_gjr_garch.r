# ------------------------------------------------------------
# Load packages
# ------------------------------------------------------------
suppressMessages(suppressWarnings(library(tseries)))
suppressMessages(suppressWarnings(library(xts)))
suppressMessages(suppressWarnings(library(rugarch)))

prices_file <- "spy_tlt.csv"

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
df     <- read.csv(prices_file, stringsAsFactors = FALSE)
dates  <- as.Date(df[[1]])      
prices <- xts(df[, 2:ncol(df)], order.by = dates)

# ------------------------------------------------------------
# Report file name, date range, and number of observations
# ------------------------------------------------------------
cat("Data file:", prices_file, "\n")
cat("Date range:", format(start(prices)), "to", format(end(prices)), "\n")
cat("Number of observations:", nrow(prices), "\n")

# ------------------------------------------------------------
# Loop over each column to fit GARCH and GJR-GARCH
# ------------------------------------------------------------
for (col_name in colnames(prices)) {
  cat("\n==============================\n")
  cat("Processing:", col_name, "\n")
  cat("==============================\n")

  # Compute log-returns and drop NA
  ret_xts <- na.omit(diff(log(prices[, col_name])))

  # Convert to numeric vector
  ret_vec <- as.numeric(ret_xts)

  # --------------------------
  # Fit GARCH(1,1) using tseries
  # --------------------------
  cat("\n--- GARCH(1,1) via tseries::garch ---\n")
  fit_garch <- suppressWarnings(
    garch(ret_vec, order = c(1, 1), trace = FALSE)
  )
  print(summary(fit_garch))

  # --------------------------
  # Fit GJR-GARCH(1,1) using rugarch
  # --------------------------
  cat("\n--- GJR-GARCH(1,1) via rugarch ---\n")
  gjr_spec <- ugarchspec(
    variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "norm"
  )
  fit_gjr <- suppressWarnings(
    ugarchfit(spec = gjr_spec, data = ret_vec, solver = "hybrid")
  )
  show(fit_gjr)
}
