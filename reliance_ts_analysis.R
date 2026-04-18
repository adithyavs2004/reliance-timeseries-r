# =============================================================================
# Time Series Analysis and Forecasting of Reliance Industries\
# =============================================================================


packages <- c(
  "quantmod", "tidyverse", "tseries", "forecast", "rugarch",
  "PerformanceAnalytics", "FinTS", "lmtest", "urca",
  "ggplot2", "gridExtra", "knitr", "xts", "zoo",
  "moments", "nortest", "car", "reshape2", "scales"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
invisible(lapply(packages, install_if_missing))
invisible(lapply(packages, library, character.only = TRUE))


# Download Reliance Industries data from Yahoo Finance
# Ticker: RELIANCE.NS (NSE) or RELIANCE.BO (BSE)
getSymbols("RELIANCE.NS", src = "yahoo", from = "2015-01-01", to = Sys.Date(), auto.assign = TRUE)

reliance_raw <- RELIANCE.NS
head(reliance_raw)
tail(reliance_raw)
dim(reliance_raw)


adj_close <- Ad(reliance_raw)
colnames(adj_close) <- "Price"

# Plot raw price time series
autoplot(adj_close) +
  ggtitle("Reliance Industries - Adjusted Closing Price") +
  xlab("Date") + ylab("Price (INR)") +
  theme_minimal()

# COMPUTE DAILY LOG RETURNS

log_returns <- na.omit(diff(log(adj_close)))
colnames(log_returns) <- "LogReturn"


lr_vec <- as.numeric(log_returns)
dates  <- index(log_returns)

cat("\n===== Summary Statistics of Log Returns =====\n")
cat("Observations :", length(lr_vec), "\n")
cat("Mean         :", round(mean(lr_vec), 6), "\n")
cat("Std Dev      :", round(sd(lr_vec), 6), "\n")
cat("Min          :", round(min(lr_vec), 6), "\n")
cat("Max          :", round(max(lr_vec), 6), "\n")
cat("Skewness     :", round(skewness(lr_vec), 4), "\n")
cat("Kurtosis     :", round(kurtosis(lr_vec), 4), "\n")

# Plot Log Returns
autoplot(log_returns) +
  ggtitle("Reliance Industries - Daily Log Returns") +
  xlab("Date") + ylab("Log Return") +
  theme_minimal()

#  DESCRIPTIVE STATISTICS & NORMALITY TESTS


# Histogram with normal overlay
hist(lr_vec, breaks = 60, probability = TRUE,
     col = "lightblue", border = "white",
     main = "Distribution of Log Returns",
     xlab = "Log Return")
curve(dnorm(x, mean(lr_vec), sd(lr_vec)), add = TRUE, col = "red", lwd = 2)

# QQ Plot
qqnorm(lr_vec, main = "QQ Plot of Log Returns")
qqline(lr_vec, col = "red")

# Normality Tests
cat("\n===== Normality Tests =====\n")
jb_test <- jarque.bera.test(lr_vec)
print(jb_test)

sw_test <- shapiro.test(sample(lr_vec, min(5000, length(lr_vec))))
print(sw_test)

ad_test <- ad.test(lr_vec)
print(ad_test)

#  STATIONARITY TESTS

cat("\n===== Stationarity Tests on Log Returns =====\n")

# Augmented Dickey-Fuller
adf_result <- adf.test(lr_vec, alternative = "stationary")
cat("\nADF Test:\n"); print(adf_result)

# KPSS Test
kpss_result <- kpss.test(lr_vec, null = "Level")
cat("\nKPSS Test:\n"); print(kpss_result)

# Phillips-Perron
pp_result <- pp.test(lr_vec)
cat("\nPhillips-Perron Test:\n"); print(pp_result)

# Also test on Price levels to contrast
cat("\n--- Stationarity Tests on Price Levels ---\n")
adf_price <- adf.test(as.numeric(adj_close), alternative = "stationary")
print(adf_price)


# ACF / PACF ANALYSIS

par(mfrow = c(2, 2))
acf(lr_vec,  main = "ACF of Log Returns",         lag.max = 40)
pacf(lr_vec, main = "PACF of Log Returns",        lag.max = 40)
acf(lr_vec^2,  main = "ACF of Squared Returns",   lag.max = 40)
pacf(lr_vec^2, main = "PACF of Squared Returns",  lag.max = 40)
par(mfrow = c(1, 1))

# Ljung-Box Test
cat("\n===== Ljung-Box Test on Returns =====\n")
lb_returns <- Box.test(lr_vec, lag = 10, type = "Ljung-Box")
print(lb_returns)

cat("\n===== Ljung-Box Test on Squared Returns (ARCH effect) =====\n")
lb_sq <- Box.test(lr_vec^2, lag = 10, type = "Ljung-Box")
print(lb_sq)

# ARCH-LM Test
cat("\n===== ARCH-LM Test =====\n")
arch_test <- ArchTest(lr_vec, lags = 12)
print(arch_test)

# -----------------------------------------------------------------------------
# 7. ARIMA MODELING
# -----------------------------------------------------------------------------
cat("\n===== Auto ARIMA Model Selection =====\n")

# Auto ARIMA
arima_auto <- auto.arima(lr_vec, stationary = TRUE, seasonal = FALSE,
                         stepwise = FALSE, approximation = FALSE,
                         ic = "aic", trace = TRUE)
summary(arima_auto)

# Residual Diagnostics
checkresiduals(arima_auto)

arima_residuals <- residuals(arima_auto)

cat("\nLjung-Box on ARIMA Residuals:\n")
print(Box.test(arima_residuals, lag = 10, type = "Ljung-Box"))

cat("\nARCH-LM Test on ARIMA Residuals:\n")
print(ArchTest(arima_residuals, lags = 12))

# Information Criteria
cat("\nAIC:", AIC(arima_auto), "  BIC:", BIC(arima_auto), "\n")


# ARIMA FORECASTING

h_arima <- 30  # 30-day forecast

arima_forecast <- forecast(arima_auto, h = h_arima)
plot(arima_forecast,
     main = "ARIMA 30-Day Forecast of Log Returns",
     xlab = "Time Index", ylab = "Log Return")

# Forecast accuracy (in-sample check using last 60 obs)
n_total   <- length(lr_vec)
train_end <- n_total - 60
train_arima <- lr_vec[1:train_end]
test_arima  <- lr_vec[(train_end + 1):n_total]

arima_train <- auto.arima(train_arima, stationary = TRUE, seasonal = FALSE)
arima_test_fc <- forecast(arima_train, h = 60)
acc_arima <- accuracy(arima_test_fc, test_arima)
cat("\n===== ARIMA Forecast Accuracy =====\n")
print(acc_arima)


# GARCH MODELING

cat("\n===== GARCH Model Fitting =====\n")

# 9a. GARCH(1,1) with normal innovations
spec_garch11 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(arima_auto$arma[1], arima_auto$arma[2]),
                        include.mean = TRUE),
  distribution.model = "norm"
)
fit_garch11 <- ugarchfit(spec = spec_garch11, data = lr_vec)
cat("\n--- GARCH(1,1) Normal ---\n")
show(fit_garch11)

# GARCH(1,1) with Student-t innovations
spec_garch_t <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(arima_auto$arma[1], arima_auto$arma[2]),
                        include.mean = TRUE),
  distribution.model = "std"
)
fit_garch_t <- ugarchfit(spec = spec_garch_t, data = lr_vec)
cat("\n--- GARCH(1,1) Student-t ---\n")
show(fit_garch_t)

# EGARCH(1,1) for asymmetric effects
spec_egarch <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(arima_auto$arma[1], arima_auto$arma[2]),
                        include.mean = TRUE),
  distribution.model = "std"
)
fit_egarch <- ugarchfit(spec = spec_egarch, data = lr_vec)
cat("\n--- EGARCH(1,1) Student-t ---\n")
show(fit_egarch)

#JR-GARCH (leverage effects)
spec_gjr <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(arima_auto$arma[1], arima_auto$arma[2]),
                        include.mean = TRUE),
  distribution.model = "std"
)
fit_gjr <- ugarchfit(spec = spec_gjr, data = lr_vec)
cat("\n--- GJR-GARCH(1,1) Student-t ---\n")
show(fit_gjr)

# Model Comparison by Information Criteria
cat("\n===== GARCH Model Comparison =====\n")
aic_g11  <- infocriteria(fit_garch11)[1]
aic_gt   <- infocriteria(fit_garch_t)[1]
aic_eg   <- infocriteria(fit_egarch)[1]
aic_gjr  <- infocriteria(fit_gjr)[1]

bic_g11  <- infocriteria(fit_garch11)[2]
bic_gt   <- infocriteria(fit_garch_t)[2]
bic_eg   <- infocriteria(fit_egarch)[2]
bic_gjr  <- infocriteria(fit_gjr)[2]

model_compare <- data.frame(
  Model = c("GARCH(1,1)-Norm", "GARCH(1,1)-t", "EGARCH(1,1)-t", "GJR-GARCH(1,1)-t"),
  AIC   = c(aic_g11, aic_gt, aic_eg, aic_gjr),
  BIC   = c(bic_g11, bic_gt, bic_eg, bic_gjr)
)
print(model_compare)

# Select best model (lowest AIC)
best_model_idx <- which.min(model_compare$AIC)
cat("\nBest Model by AIC:", model_compare$Model[best_model_idx], "\n")

# Pick best fit for further analysis (using GARCH-t as robust default)
best_fit <- fit_garch_t

# Conditional Volatility Plot
cond_vol <- sigma(best_fit)
cond_vol_xts <- xts(cond_vol, order.by = dates)

autoplot(cond_vol_xts) +
  ggtitle("GARCH(1,1)-t Conditional Volatility") +
  xlab("Date") + ylab("Conditional Std Dev") +
  theme_minimal()

# GARCH Residual Diagnostics
garch_std_res <- residuals(best_fit, standardize = TRUE)
cat("\nLjung-Box on Standardized GARCH Residuals:\n")
print(Box.test(garch_std_res, lag = 10, type = "Ljung-Box"))
cat("\nLjung-Box on Squared Std Residuals:\n")
print(Box.test(garch_std_res^2, lag = 10, type = "Ljung-Box"))


# GARCH FORECASTING

h_garch <- 30
garch_forecast <- ugarchforecast(best_fit, n.ahead = h_garch)
cat("\n===== GARCH 30-Day Volatility Forecast =====\n")
print(sigma(garch_forecast))

plot(garch_forecast, which = 3)  # Conditional volatility forecast
plot(garch_forecast, which = 1)  # Series forecast


#FINANCIAL RISK METRICS

cat("\n===== Financial Risk Metrics =====\n")

# Annualised return and volatility (252 trading days)
ann_return <- mean(lr_vec) * 252
ann_vol    <- sd(lr_vec) * sqrt(252)

cat("Annualised Return   :", round(ann_return * 100, 2), "%\n")
cat("Annualised Volatility:", round(ann_vol * 100, 2), "%\n")

# Sharpe Ratio (assume risk-free rate = 6.5% p.a., i.e., India repo rate)
rf_daily   <- 0.065 / 252
sharpe     <- (mean(lr_vec) - rf_daily) / sd(lr_vec) * sqrt(252)
cat("Sharpe Ratio (annualised):", round(sharpe, 4), "\n")

# Value at Risk (Historical Simulation)
var_95 <- quantile(lr_vec, 0.05)
var_99 <- quantile(lr_vec, 0.01)
cat("VaR (95%, 1-day):", round(var_95 * 100, 3), "%\n")
cat("VaR (99%, 1-day):", round(var_99 * 100, 3), "%\n")

# Conditional VaR / Expected Shortfall
cvar_95 <- mean(lr_vec[lr_vec <= var_95])
cvar_99 <- mean(lr_vec[lr_vec <= var_99])
cat("CVaR / ES (95%)  :", round(cvar_95 * 100, 3), "%\n")
cat("CVaR / ES (99%)  :", round(cvar_99 * 100, 3), "%\n")

# GARCH-based VaR
garch_var_95 <- quantile(as.numeric(garch_std_res), 0.05) * tail(cond_vol, 1)
cat("GARCH VaR (95%, 1-day):", round(as.numeric(garch_var_95) * 100, 3), "%\n")

# Maximum Drawdown
wealth_index <- cumprod(1 + lr_vec)
drawdown     <- (wealth_index - cummax(wealth_index)) / cummax(wealth_index)
max_dd       <- min(drawdown)
cat("Maximum Drawdown:", round(max_dd * 100, 2), "%\n")

# Sortino Ratio
negative_returns <- lr_vec[lr_vec < 0]
downside_dev     <- sqrt(mean(negative_returns^2)) * sqrt(252)
sortino <- (ann_return - 0.065) / downside_dev
cat("Sortino Ratio:", round(sortino, 4), "\n")

# Rolling 30-day Volatility
rolling_vol <- rollapply(log_returns, width = 30, FUN = sd, fill = NA, align = "right") * sqrt(252)
colnames(rolling_vol) <- "RollingVol"
autoplot(rolling_vol) +
  ggtitle("Rolling 30-Day Annualised Volatility") +
  xlab("Date") + ylab("Annualised Volatility") +
  theme_minimal()


# RETURNS DECOMPOSITION (Optional: Structural Break)

# Rolling mean return (60-day)
rolling_mean <- rollapply(log_returns, width = 60, FUN = mean, fill = NA, align = "right") * 252
colnames(rolling_mean) <- "RollingMean"

p1 <- autoplot(log_returns) +
  ggtitle("Daily Log Returns") + theme_minimal()
p2 <- autoplot(rolling_mean) +
  ggtitle("Rolling 60-Day Mean Return (Annualised)") + theme_minimal()
p3 <- autoplot(rolling_vol) +
  ggtitle("Rolling 30-Day Vol (Annualised)") + theme_minimal()
grid.arrange(p1, p2, p3, ncol = 1)


#  SAVE OUTPUTS FOR R MARKDOWN

save(
  adj_close, log_returns, lr_vec, dates,
  arima_auto, arima_forecast, acc_arima,
  fit_garch11, fit_garch_t, fit_egarch, fit_gjr,
  best_fit, garch_forecast, cond_vol,
  model_compare, garch_std_res,
  ann_return, ann_vol, sharpe, var_95, var_99,
  cvar_95, cvar_99, max_dd, sortino,
  rolling_vol, rolling_mean,
  file = "reliance_ts_results.RData"
)
cat("\nAll results saved to reliance_ts_results.RData\n")

