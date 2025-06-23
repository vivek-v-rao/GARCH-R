suppressMessages(suppressWarnings(library(rugarch)))

set.seed(42)                       # reproducibility
nobs <- 10000                      # length of series

# ------------------------------------------------------------
# 1.  True model specification and simulation
# ------------------------------------------------------------
true_pars <- list(omega  = 1e-6,
                  alpha1 = 0.05,
                  gamma1 = 0.70,   # leverage term
                  beta1  = 0.10)

sim.spec <- ugarchspec(
  variance.model = list(model      = "gjrGARCH",
                        garchOrder = c(1, 1)),
  mean.model     = list(armaOrder  = c(0, 0),
                        include.mean = FALSE),
  fixed.pars     = true_pars)

sim.path <- ugarchpath(sim.spec, n.sim = nobs)
returns  <- as.numeric(fitted(sim.path))  # ε_t series

# ------------------------------------------------------------
# 2.  Fit the same model (no parameters fixed)
# ------------------------------------------------------------
fit.spec <- ugarchspec(
  variance.model = list(model      = "gjrGARCH",
                        garchOrder = c(1, 1)),
  mean.model     = list(armaOrder  = c(0, 0),
                        include.mean = FALSE))

fit <- ugarchfit(spec = fit.spec, data = returns, solver = "hybrid")

# ------------------------------------------------------------
# 3.  Build comparison data.frame
# ------------------------------------------------------------
est_pars <- coef(fit)[c("omega", "alpha1", "gamma1", "beta1")]

param_df <- data.frame(
  param     = names(true_pars),
  true      = unlist(true_pars, use.names = FALSE),
  estimated = as.numeric(est_pars),
  row.names = NULL
)

print(param_df, row.names=FALSE)
