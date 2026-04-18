# Time Series Analysis and Forecasting of Reliance Industries

A complete time series project built in R, analysing 10+ years of daily stock data for Reliance Industries Limited (NSE: RELIANCE.NS). The project covers statistical testing, ARIMA and GARCH modelling, financial risk assessment, and a fully interactive Shiny dashboard.

---

## Table of Contents

- [Overview](#overview)
- [Project Structure](#project-structure)
- [Installation](#installation)
- [How to Run](#how-to-run)
- [Analysis Pipeline](#analysis-pipeline)
- [Models Used](#models-used)
- [Risk Metrics](#risk-metrics)
- [Key Findings](#key-findings)
- [Tools and Packages](#tools-and-packages)
- [Author](#author)

---

## Overview

Reliance Industries Limited (RIL) is India's largest private sector company and a core constituent of the NIFTY 50 and Sensex. This project analyses its daily adjusted closing prices from January 2015 to April 2026 (2,788 observations) using a rigorous time series framework.

The goal is to understand the return structure, model volatility, generate short-term forecasts, and quantify financial risk — all implemented end-to-end in R.

---

## Project Structure

```
reliance-timeseries-r/
│
├── reliance_ts_analysis.R      # Full standalone analysis script
├── reliance_ts_report.Rmd      # knitr R Markdown report (knits to HTML)
├── reliance_shiny_app.R        # Interactive Shiny dashboard
└── README.md
```

| File | Description |
|------|-------------|
| `reliance_ts_analysis.R` | Downloads data, runs all tests, fits ARIMA and GARCH models, computes risk metrics, saves results |
| `reliance_ts_report.Rmd` | Full reproducible report with plots, tables and interpretations — knits to a clean HTML file |
| `reliance_shiny_app.R` | Interactive dashboard with 7 tabs, live model controls and real-time forecasting |

---

## Installation

Install all required R packages by running this once in your R console:

```r
packages <- c(
  "quantmod", "tidyverse", "tseries", "forecast", "rugarch",
  "PerformanceAnalytics", "FinTS", "lmtest", "urca",
  "ggplot2", "gridExtra", "knitr", "xts", "zoo",
  "moments", "nortest", "car", "reshape2", "scales",
  "shiny", "shinydashboard", "plotly", "DT", "kableExtra"
)
install.packages(packages)
```

**Requirements:**
- R version 4.0 or higher
- RStudio (recommended)
- Internet connection (data is pulled live from Yahoo Finance via `quantmod`)

---

## How to Run

### Option 1 — Run the Analysis Script

```r
source("reliance_ts_analysis.R")
```

Runs the full pipeline and saves all model outputs to `reliance_ts_results.RData`.

### Option 2 — Knit the R Markdown Report

```r
rmarkdown::render("reliance_ts_report.Rmd")
```

Produces `reliance_ts_report.html` — a self-contained report with all plots, tables, and code.

### Option 3 — Launch the Shiny Dashboard

```r
shiny::runApp("reliance_shiny_app.R")
```

Opens an interactive dashboard in your browser. Use the sidebar to change the date range, forecast horizon, GARCH model type, and innovation distribution.

---

## Analysis Pipeline

```
Yahoo Finance (quantmod)
        |
        v
Adjusted Close Prices (RELIANCE.NS, 2015-2026)
        |
        v
Log Returns  -->  Descriptive Statistics  -->  Normality Tests
        |
        v
Stationarity Tests (ADF, KPSS, Phillips-Perron)
        |
        v
ACF / PACF  -->  Ljung-Box Test  -->  ARCH-LM Test
        |
        v
Auto ARIMA  -->  Residual Diagnostics  -->  30-Day Forecast
        |
        v
GARCH Family  -->  Model Comparison (AIC / BIC)
        |
        v
Conditional Volatility  -->  30-Day Volatility Forecast
        |
        v
Risk Metrics (VaR, CVaR, Sharpe, Sortino, Max Drawdown)
```

---

## Models Used

| Model | Purpose |
|-------|---------|
| ARIMA(p,0,q) | Captures linear autocorrelation in returns |
| GARCH(1,1) Normal | Baseline conditional volatility model |
| GARCH(1,1) Student-t | Accounts for fat tails in return distribution |
| EGARCH(1,1) Student-t | Captures asymmetric (leverage) effects |
| GJR-GARCH(1,1) Student-t | Captures leverage effects (Glosten-Jagannathan-Runkle) |

Model selection is based on AIC and BIC. The best model is highlighted automatically in the report and dashboard.

---

## Risk Metrics

| Metric | Description |
|--------|-------------|
| VaR (95%, 99%) | Maximum expected loss at a given confidence level over 1 day |
| CVaR / Expected Shortfall | Average loss beyond the VaR threshold |
| Sharpe Ratio | Risk-adjusted return relative to a risk-free rate of 6.5% p.a. |
| Sortino Ratio | Like Sharpe but only penalises downside volatility |
| Maximum Drawdown | Largest peak-to-trough decline over the full period |
| Rolling Volatility | 30-day rolling annualised standard deviation |

---

## Key Findings

- **Non-normality:** All three normality tests (Jarque-Bera, Anderson-Darling, Shapiro-Wilk) strongly reject the normal distribution. Returns show fat tails and negative skewness.
- **Stationarity:** Price levels contain a unit root. Log returns are stationary, confirmed by ADF, KPSS and Phillips-Perron tests.
- **Volatility Clustering:** ARCH-LM and Ljung-Box tests on squared returns confirm strong ARCH effects, justifying GARCH modelling.
- **Best Model:** GARCH(1,1) with Student-t innovations consistently outperforms other models on AIC and BIC, capturing fat tails and high volatility persistence.
- **Risk:** The high alpha + beta sum in the GARCH model indicates that volatility shocks to Reliance are long-lasting.

---

## Shiny Dashboard Tabs

| Tab | Content |
|-----|---------|
| Overview | Latest price, annualised return, volatility, Sharpe ratio, price chart |
| Returns | Log returns plot, histogram, descriptive stats, normality tests |
| Stationarity | ACF/PACF plots, stationarity and ARCH test results |
| ARIMA | Model summary, residual diagnostics, forecast accuracy |
| GARCH | Model comparison table, conditional volatility, coefficient table, residual ACF |
| Risk | VaR/CVaR value boxes, risk metrics table, VaR plot, rolling volatility |
| Forecasting | ARIMA return forecast, GARCH volatility forecast, forecast table |

---

## Tools and Packages

| Category | Packages |
|----------|----------|
| Data | quantmod, xts, zoo |
| Statistical Tests | tseries, FinTS, nortest, moments, urca |
| Modelling | forecast, rugarch |
| Visualisation | ggplot2, plotly, gridExtra, scales |
| Reporting | knitr, rmarkdown, kableExtra |
| Dashboard | shiny, shinydashboard, DT |

---

## Author

**Adithya V S**

[![LinkedIn](https://img.shields.io/badge/LinkedIn-Connect-blue?style=flat&logo=linkedin)](https://www.linkedin.com/in/adithyavinodsangeetha/)
[![Email](https://img.shields.io/badge/Email-Contact-red?style=flat&logo=gmail)](mailto:adithyavspunartham@gmail.com)

---

## References

- Engle, R. F. (1982). Autoregressive Conditional Heteroscedasticity. *Econometrica*, 50(4), 987-1007.
- Bollerslev, T. (1986). Generalized Autoregressive Conditional Heteroscedasticity. *Journal of Econometrics*, 31(3), 307-327.
- Box, G. E. P., Jenkins, G. M., and Reinsel, G. C. (2015). *Time Series Analysis: Forecasting and Control*. Wiley.
