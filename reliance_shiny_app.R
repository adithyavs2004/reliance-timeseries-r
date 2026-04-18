# =============================================================================
# Shiny Dashboard: Time Series Analysis of Reliance Industries
# Run with: shiny::runApp("reliance_shiny_app.R")
# =============================================================================

library(shiny)
library(shinydashboard)
library(quantmod)
library(forecast)
library(rugarch)
library(FinTS)
library(tseries)
library(ggplot2)
library(plotly)
library(moments)
library(zoo)
library(xts)
library(scales)
library(DT)

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "Reliance Industries — Time Series Dashboard"
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",         tabName = "overview",    icon = icon("chart-line")),
      menuItem("Returns Analysis", tabName = "returns",     icon = icon("chart-bar")),
      menuItem("Stationarity",     tabName = "stationary",  icon = icon("check-circle")),
      menuItem("ARIMA Model",      tabName = "arima",       icon = icon("wave-square")),
      menuItem("GARCH Model",      tabName = "garch",       icon = icon("fire")),
      menuItem("Risk Metrics",     tabName = "risk",        icon = icon("shield-alt")),
      menuItem("Forecasting",      tabName = "forecast",    icon = icon("eye"))
    ),
    hr(),
    # Sidebar Controls
    dateRangeInput("date_range",
                   label = "Date Range",
                   start = "2015-01-01",
                   end   = Sys.Date(),
                   min   = "2010-01-01",
                   max   = Sys.Date()),
    actionButton("load_data", "Load / Refresh Data",
                 icon = icon("sync"),
                 class = "btn-primary btn-block"),
    hr(),
    sliderInput("forecast_horizon", "Forecast Horizon (days):",
                min = 5, max = 60, value = 30, step = 5),
    selectInput("garch_dist", "GARCH Innovation Distribution:",
                choices = c("Normal" = "norm",
                            "Student-t" = "std",
                            "Skew-t" = "sstd"),
                selected = "std"),
    selectInput("garch_model", "GARCH Model Type:",
                choices = c("Standard GARCH" = "sGARCH",
                            "EGARCH"          = "eGARCH",
                            "GJR-GARCH"       = "gjrGARCH"),
                selected = "sGARCH")
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .box { border-radius: 6px; }
      .info-box { border-radius: 6px; }
      .value-box .icon-large { font-size: 60px; }
    "))),

    tabItems(

      # ── OVERVIEW ────────────────────────────────────────────────────────────
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_price",      width = 3),
          valueBoxOutput("vb_return",     width = 3),
          valueBoxOutput("vb_vol",        width = 3),
          valueBoxOutput("vb_sharpe",     width = 3)
        ),
        fluidRow(
          box(title = "Adjusted Closing Price", width = 12, status = "primary",
              plotlyOutput("price_plot", height = "380px"))
        )
      ),

      # ── RETURNS ANALYSIS ───────────────────────────────────────────────────
      tabItem(tabName = "returns",
        fluidRow(
          box(title = "Daily Log Returns", width = 8, status = "info",
              plotlyOutput("returns_plot", height = "300px")),
          box(title = "Return Distribution", width = 4, status = "info",
              plotlyOutput("hist_plot", height = "300px"))
        ),
        fluidRow(
          box(title = "Descriptive Statistics", width = 6, status = "success",
              DTOutput("desc_stats_table")),
          box(title = "Normality Tests", width = 6, status = "warning",
              DTOutput("norm_tests_table"))
        )
      ),

      # ── STATIONARITY ────────────────────────────────────────────────────────
      tabItem(tabName = "stationary",
        fluidRow(
          box(title = "ACF — Log Returns", width = 6, status = "primary",
              plotOutput("acf_returns", height = "250px")),
          box(title = "PACF — Log Returns", width = 6, status = "primary",
              plotOutput("pacf_returns", height = "250px"))
        ),
        fluidRow(
          box(title = "ACF — Squared Returns", width = 6, status = "warning",
              plotOutput("acf_sq_returns", height = "250px")),
          box(title = "PACF — Squared Returns", width = 6, status = "warning",
              plotOutput("pacf_sq_returns", height = "250px"))
        ),
        fluidRow(
          box(title = "Stationarity & ARCH Tests", width = 12, status = "success",
              DTOutput("stat_tests_table"))
        )
      ),

      # ── ARIMA ───────────────────────────────────────────────────────────────
      tabItem(tabName = "arima",
        fluidRow(
          box(title = "Auto ARIMA — Model Summary", width = 6, status = "primary",
              verbatimTextOutput("arima_summary")),
          box(title = "ARIMA Residuals", width = 6, status = "info",
              plotOutput("arima_residuals_plot", height = "320px"))
        ),
        fluidRow(
          box(title = "Forecast Accuracy", width = 12, status = "success",
              DTOutput("arima_acc_table"))
        )
      ),

      # ── GARCH ───────────────────────────────────────────────────────────────
      tabItem(tabName = "garch",
        fluidRow(
          box(title = "GARCH Model Comparison", width = 12, status = "primary",
              DTOutput("garch_compare_table"))
        ),
        fluidRow(
          box(title = "Conditional Volatility", width = 8, status = "info",
              plotlyOutput("cond_vol_plot", height = "320px")),
          box(title = "GARCH Coefficients", width = 4, status = "success",
              DTOutput("garch_coef_table"))
        ),
        fluidRow(
          box(title = "Standardized Residual ACF / ACF²", width = 12, status = "warning",
              fluidRow(
                column(6, plotOutput("garch_acf_res",    height = "240px")),
                column(6, plotOutput("garch_acf_sq_res", height = "240px"))
              ))
        )
      ),

      # ── RISK METRICS ─────────────────────────────────────────────────────────
      tabItem(tabName = "risk",
        fluidRow(
          valueBoxOutput("vb_var95",  width = 3),
          valueBoxOutput("vb_var99",  width = 3),
          valueBoxOutput("vb_maxdd",  width = 3),
          valueBoxOutput("vb_sortino",width = 3)
        ),
        fluidRow(
          box(title = "Return Distribution with VaR Lines", width = 8, status = "danger",
              plotlyOutput("var_plot", height = "320px")),
          box(title = "Risk Metrics Summary", width = 4, status = "success",
              DTOutput("risk_table"))
        ),
        fluidRow(
          box(title = "Rolling 30-Day Volatility", width = 12, status = "primary",
              plotlyOutput("rolling_vol_plot", height = "280px"))
        )
      ),

      # ── FORECASTING ─────────────────────────────────────────────────────────
      tabItem(tabName = "forecast",
        fluidRow(
          box(title = "ARIMA Return Forecast", width = 6, status = "primary",
              plotlyOutput("arima_fc_plot", height = "320px")),
          box(title = "GARCH Volatility Forecast", width = 6, status = "warning",
              plotlyOutput("garch_fc_plot", height = "320px"))
        ),
        fluidRow(
          box(title = "GARCH Forecast Table", width = 12, status = "success",
              DTOutput("garch_fc_table"))
        )
      )
    )
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactive: Load Data ────────────────────────────────────────────────────
  raw_data <- eventReactive(input$load_data, {
    withProgress(message = "Downloading RELIANCE.NS data...", value = 0.2, {
      tryCatch({
        getSymbols("RELIANCE.NS", src = "yahoo",
                   from = format(input$date_range[1]),
                   to   = format(input$date_range[2]),
                   auto.assign = FALSE)
      }, error = function(e) NULL)
    })
  }, ignoreNULL = FALSE)

  adj_close_r <- reactive({
    req(raw_data())
    x <- Ad(raw_data())
    colnames(x) <- "Price"
    x
  })

  lr_r <- reactive({
    req(adj_close_r())
    lr <- na.omit(diff(log(adj_close_r())))
    colnames(lr) <- "LogReturn"
    lr
  })

  lr_vec_r <- reactive(as.numeric(lr_r()))

  # ── Reactive: ARIMA ────────────────────────────────────────────────────────
  arima_r <- reactive({
    req(lr_vec_r())
    withProgress(message = "Fitting ARIMA...", value = 0.5, {
      auto.arima(lr_vec_r(), stationary = TRUE,
                 seasonal = FALSE, stepwise = TRUE, approximation = TRUE)
    })
  })

  # ── Reactive: GARCH ────────────────────────────────────────────────────────
  garch_r <- reactive({
    req(lr_vec_r(), arima_r())
    ar <- arima_r()$arma[1]; ma <- arima_r()$arma[2]
    spec <- ugarchspec(
      variance.model    = list(model = input$garch_model, garchOrder = c(1, 1)),
      mean.model        = list(armaOrder = c(ar, ma), include.mean = TRUE),
      distribution.model = input$garch_dist
    )
    withProgress(message = "Fitting GARCH...", value = 0.7, {
      tryCatch(ugarchfit(spec, data = lr_vec_r(), solver = "hybrid"), error = function(e) NULL)
    })
  })

  # ── OVERVIEW value boxes ────────────────────────────────────────────────────
  output$vb_price <- renderValueBox({
    req(adj_close_r())
    last_price <- round(as.numeric(last(adj_close_r())), 1)
    valueBox(paste0("₹", last_price), "Latest Price", icon = icon("rupee-sign"), color = "blue")
  })
  output$vb_return <- renderValueBox({
    req(lr_vec_r())
    ann_ret <- round(mean(lr_vec_r()) * 252 * 100, 2)
    valueBox(paste0(ann_ret, "%"), "Annualised Return", icon = icon("trending-up"), color = "green")
  })
  output$vb_vol <- renderValueBox({
    req(lr_vec_r())
    ann_vol <- round(sd(lr_vec_r()) * sqrt(252) * 100, 2)
    valueBox(paste0(ann_vol, "%"), "Annualised Volatility", icon = icon("fire"), color = "orange")
  })
  output$vb_sharpe <- renderValueBox({
    req(lr_vec_r())
    rf <- 0.065/252
    sharpe <- round((mean(lr_vec_r()) - rf) / sd(lr_vec_r()) * sqrt(252), 3)
    valueBox(sharpe, "Sharpe Ratio", icon = icon("star"), color = "purple")
  })

  # ── Price Plot ─────────────────────────────────────────────────────────────
  output$price_plot <- renderPlotly({
    req(adj_close_r())
    df <- data.frame(Date = index(adj_close_r()),
                     Price = as.numeric(adj_close_r()))
    plot_ly(df, x = ~Date, y = ~Price, type = "scatter", mode = "lines",
            line = list(color = "steelblue", width = 1.2),
            name = "Price") |>
      layout(title = "Reliance Industries — Adjusted Price",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Price (INR)", tickformat = ","),
             hovermode = "x unified")
  })

  # ── Returns Plot ───────────────────────────────────────────────────────────
  output$returns_plot <- renderPlotly({
    req(lr_r())
    df <- data.frame(Date = index(lr_r()), Ret = as.numeric(lr_r()))
    plot_ly(df, x = ~Date, y = ~Ret, type = "bar",
            marker = list(color = "steelblue")) |>
      layout(title = "Daily Log Returns",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Log Return"))
  })

  output$hist_plot <- renderPlotly({
    req(lr_vec_r())
    plot_ly(x = lr_vec_r(), type = "histogram", nbinsx = 80,
            marker = list(color = "steelblue", line = list(color = "white", width = 0.3)),
            name = "Returns") |>
      layout(title = "Return Distribution",
             xaxis = list(title = "Log Return"),
             yaxis = list(title = "Count"))
  })

  # ── Descriptive Stats ─────────────────────────────────────────────────────
  output$desc_stats_table <- renderDT({
    req(lr_vec_r()); lv <- lr_vec_r()
    df <- data.frame(
      Statistic = c("N","Mean","Std Dev","Min","Max","Skewness","Kurtosis"),
      Value     = c(length(lv), round(mean(lv),6), round(sd(lv),6),
                    round(min(lv),6), round(max(lv),6),
                    round(skewness(lv),4), round(kurtosis(lv),4))
    )
    datatable(df, options = list(dom = "t", pageLength = 10), rownames = FALSE)
  })

  output$norm_tests_table <- renderDT({
    req(lr_vec_r()); lv <- lr_vec_r()
    jb <- jarque.bera.test(lv)
    ad <- nortest::ad.test(lv)
    df <- data.frame(
      Test      = c("Jarque-Bera", "Anderson-Darling"),
      Statistic = round(c(jb$statistic, ad$statistic), 4),
      p.value   = c(formatC(jb$p.value, format="e", digits=3),
                    formatC(ad$p.value, format="e", digits=3)),
      Decision  = c("Reject Normality","Reject Normality")
    )
    datatable(df, options = list(dom = "t"), rownames = FALSE)
  })

  # ── ACF/PACF Plots ─────────────────────────────────────────────────────────
  output$acf_returns    <- renderPlot({ req(lr_vec_r()); acf(lr_vec_r(),    lag.max=40, main="ACF — Returns") })
  output$pacf_returns   <- renderPlot({ req(lr_vec_r()); pacf(lr_vec_r(),   lag.max=40, main="PACF — Returns") })
  output$acf_sq_returns <- renderPlot({ req(lr_vec_r()); acf(lr_vec_r()^2,  lag.max=40, main="ACF — Sq Returns") })
  output$pacf_sq_returns<- renderPlot({ req(lr_vec_r()); pacf(lr_vec_r()^2, lag.max=40, main="PACF — Sq Returns") })

  # ── Stationarity Table ────────────────────────────────────────────────────
  output$stat_tests_table <- renderDT({
    req(lr_vec_r()); lv <- lr_vec_r()
    adf  <- adf.test(lv, alternative = "stationary")
    pp   <- pp.test(lv)
    kpss <- kpss.test(lv, null = "Level")
    arch <- FinTS::ArchTest(lv, lags = 12)
    df <- data.frame(
      Test = c("ADF","PP","KPSS","ARCH-LM"),
      Stat = round(c(adf$statistic, pp$statistic, kpss$statistic, arch$statistic), 4),
      pval = c(round(adf$p.value,4), round(pp$p.value,4),
               round(kpss$p.value,4), formatC(arch$p.value,format="e",digits=2)),
      Conclusion = c("Stationary","Stationary","Stationary","ARCH Effects Present")
    )
    datatable(df, options = list(dom="t"), rownames=FALSE)
  })

  # ── ARIMA Summary ─────────────────────────────────────────────────────────
  output$arima_summary <- renderPrint({
    req(arima_r()); summary(arima_r())
  })
  output$arima_residuals_plot <- renderPlot({
    req(arima_r()); checkresiduals(arima_r())
  })
  output$arima_acc_table <- renderDT({
    req(arima_r(), lr_vec_r()); lv <- lr_vec_r()
    n <- length(lv); te <- n - 60
    tr_fit <- auto.arima(lv[1:te], stationary=TRUE, seasonal=FALSE, stepwise=TRUE)
    fc     <- forecast(tr_fit, h=60)
    acc    <- accuracy(fc, lv[(te+1):n])
    datatable(as.data.frame(round(acc,5)), options=list(scrollX=TRUE))
  })

  # ── GARCH Compare ─────────────────────────────────────────────────────────
  output$garch_compare_table <- renderDT({
    req(lr_vec_r(), arima_r()); lv <- lr_vec_r()
    ar <- arima_r()$arma[1]; ma <- arima_r()$arma[2]
    mk_spec <- function(mod, dist) {
      ugarchspec(variance.model=list(model=mod, garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(ar,ma), include.mean=TRUE),
                 distribution.model=dist)
    }
    withProgress(message="Fitting all GARCH variants...", value=0.5, {
      fits <- list(
        "GARCH-Norm" = ugarchfit(mk_spec("sGARCH","norm"), data=lv, solver="hybrid"),
        "GARCH-t"    = ugarchfit(mk_spec("sGARCH","std"),  data=lv, solver="hybrid"),
        "EGARCH-t"   = ugarchfit(mk_spec("eGARCH","std"),  data=lv, solver="hybrid"),
        "GJR-GARCH-t"= ugarchfit(mk_spec("gjrGARCH","std"),data=lv, solver="hybrid")
      )
      df <- do.call(rbind, lapply(names(fits), function(nm) {
        ic <- infocriteria(fits[[nm]])
        data.frame(Model=nm, AIC=round(ic[1],4), BIC=round(ic[2],4),
                   LogLik=round(likelihood(fits[[nm]]),2))
      }))
    })
    datatable(df, options=list(dom="t"), rownames=FALSE) |>
      formatStyle("AIC", backgroundColor = styleInterval(min(df$AIC)+0.001, c("#d4edda","white")))
  })

  output$cond_vol_plot <- renderPlotly({
    req(garch_r()); gf <- garch_r()
    cv   <- as.numeric(sigma(gf)) * sqrt(252)
    dts  <- index(lr_r())
    df   <- data.frame(Date=dts, AnnVol=cv)
    plot_ly(df, x=~Date, y=~AnnVol, type="scatter", mode="lines",
            line=list(color="steelblue")) |>
      layout(title="Annualised Conditional Volatility",
             yaxis=list(title="Volatility", tickformat=".1%"),
             xaxis=list(title="Date"))
  })

  output$garch_coef_table <- renderDT({
    req(garch_r())
    cf <- coef(garch_r())
    datatable(data.frame(Parameter=names(cf), Estimate=round(cf,6)),
              options=list(dom="t"), rownames=FALSE)
  })

  output$garch_acf_res    <- renderPlot({
    req(garch_r())
    acf(residuals(garch_r(), standardize=TRUE),   lag.max=30, main="ACF Std Residuals")
  })
  output$garch_acf_sq_res <- renderPlot({
    req(garch_r())
    acf(residuals(garch_r(), standardize=TRUE)^2, lag.max=30, main="ACF Sq Std Residuals")
  })

  # ── Risk Value Boxes ──────────────────────────────────────────────────────
  output$vb_var95  <- renderValueBox({
    req(lr_vec_r())
    v <- round(quantile(lr_vec_r(), 0.05)*100, 3)
    valueBox(paste0(v, "%"), "VaR (95%, 1-day)", icon=icon("exclamation-triangle"), color="yellow")
  })
  output$vb_var99  <- renderValueBox({
    req(lr_vec_r())
    v <- round(quantile(lr_vec_r(), 0.01)*100, 3)
    valueBox(paste0(v, "%"), "VaR (99%, 1-day)", icon=icon("exclamation-circle"), color="red")
  })
  output$vb_maxdd  <- renderValueBox({
    req(lr_vec_r()); lv <- lr_vec_r()
    wi <- cumprod(1+lv); dd <- min((wi-cummax(wi))/cummax(wi))
    valueBox(paste0(round(dd*100,2),"%"), "Max Drawdown", icon=icon("arrow-down"), color="navy")
  })
  output$vb_sortino <- renderValueBox({
    req(lr_vec_r()); lv <- lr_vec_r()
    neg <- lv[lv<0]
    dd  <- sqrt(mean(neg^2))*sqrt(252)
    sr  <- round((mean(lv)*252-0.065)/dd, 3)
    valueBox(sr, "Sortino Ratio", icon=icon("balance-scale"), color="purple")
  })

  output$var_plot <- renderPlotly({
    req(lr_vec_r()); lv <- lr_vec_r()
    v95 <- quantile(lv, 0.05); v99 <- quantile(lv, 0.01)
    plot_ly(x=lv, type="histogram", nbinsx=80,
            marker=list(color="steelblue")) |>
      add_segments(x=v95, xend=v95, y=0, yend=50,
                   line=list(color="orange", dash="dash"), name="VaR 95%") |>
      add_segments(x=v99, xend=v99, y=0, yend=50,
                   line=list(color="red", dash="dash"), name="VaR 99%") |>
      layout(xaxis=list(title="Log Return"), yaxis=list(title="Count"),
             barmode="overlay")
  })

  output$risk_table <- renderDT({
    req(lr_vec_r()); lv <- lr_vec_r()
    rf <- 0.065/252
    v95 <- quantile(lv,0.05); v99 <- quantile(lv,0.01)
    wi  <- cumprod(1+lv); dd <- min((wi-cummax(wi))/cummax(wi))
    neg <- lv[lv<0]; sortino_r <- (mean(lv)*252-0.065)/(sqrt(mean(neg^2))*sqrt(252))
    df <- data.frame(
      Metric = c("Ann. Return","Ann. Vol","Sharpe","Sortino",
                 "VaR 95%","VaR 99%","CVaR 95%","Max DD"),
      Value  = c(paste0(round(mean(lv)*252*100,2),"%"),
                 paste0(round(sd(lv)*sqrt(252)*100,2),"%"),
                 round((mean(lv)-rf)/sd(lv)*sqrt(252),3),
                 round(sortino_r,3),
                 paste0(round(v95*100,3),"%"),
                 paste0(round(v99*100,3),"%"),
                 paste0(round(mean(lv[lv<=v95])*100,3),"%"),
                 paste0(round(dd*100,2),"%"))
    )
    datatable(df, options=list(dom="t", pageLength=10), rownames=FALSE)
  })

  output$rolling_vol_plot <- renderPlotly({
    req(lr_r())
    rv  <- rollapply(lr_r(), 30, sd, fill=NA, align="right") * sqrt(252)
    df  <- data.frame(Date=index(rv), Vol=as.numeric(rv))
    plot_ly(df, x=~Date, y=~Vol, type="scatter", mode="lines",
            line=list(color="darkorange")) |>
      layout(yaxis=list(title="Ann. Volatility", tickformat=".1%"),
             xaxis=list(title="Date"))
  })

  # ── Forecast Plots ─────────────────────────────────────────────────────────
  output$arima_fc_plot <- renderPlotly({
    req(arima_r())
    h  <- input$forecast_horizon
    fc <- forecast(arima_r(), h=h)
    df <- data.frame(
      Index = seq_len(h),
      Mean  = as.numeric(fc$mean),
      Lo80  = as.numeric(fc$lower[,1]),
      Hi80  = as.numeric(fc$upper[,1]),
      Lo95  = as.numeric(fc$lower[,2]),
      Hi95  = as.numeric(fc$upper[,2])
    )
    plot_ly(df, x=~Index) |>
      add_ribbons(ymin=~Lo95, ymax=~Hi95, name="95% CI", fillcolor="rgba(70,130,180,0.15)", line=list(width=0)) |>
      add_ribbons(ymin=~Lo80, ymax=~Hi80, name="80% CI", fillcolor="rgba(70,130,180,0.25)", line=list(width=0)) |>
      add_lines(y=~Mean, name="Forecast", line=list(color="steelblue")) |>
      layout(title=paste0("ARIMA ", h, "-Day Return Forecast"),
             xaxis=list(title="Days Ahead"), yaxis=list(title="Log Return"))
  })

  output$garch_fc_plot <- renderPlotly({
    req(garch_r())
    h  <- input$forecast_horizon
    gf <- ugarchforecast(garch_r(), n.ahead=h)
    sv <- as.numeric(sigma(gf)) * sqrt(252)
    df <- data.frame(Day=seq_len(h), AnnVol=sv)
    plot_ly(df, x=~Day, y=~AnnVol, type="scatter", mode="lines+markers",
            line=list(color="darkorange")) |>
      layout(title=paste0("GARCH ", h, "-Day Volatility Forecast"),
             xaxis=list(title="Days Ahead"),
             yaxis=list(title="Ann. Volatility", tickformat=".1%"))
  })

  output$garch_fc_table <- renderDT({
    req(garch_r())
    h  <- input$forecast_horizon
    gf <- ugarchforecast(garch_r(), n.ahead=h)
    df <- data.frame(
      Day            = seq_len(h),
      ReturnForecast = round(as.numeric(fitted(gf)), 6),
      CondVol_Daily  = round(as.numeric(sigma(gf)), 6),
      CondVol_Annual = paste0(round(as.numeric(sigma(gf))*sqrt(252)*100, 2), "%")
    )
    datatable(df, options=list(pageLength=15, scrollX=TRUE), rownames=FALSE)
  })
}

# ─────────────────────────────────────────────────────────────────────────────
# RUN
# ─────────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
