# =====================================================
# COMPLETE STOCK MARKET DASHBOARD
# FULLY AUTOMATIC - NO UPLOAD REQUIRED
# ERROR FREE VERSION
# =====================================================

# =====================================================
# STEP 1 : INSTALL PACKAGES
# RUN ONLY FIRST TIME
# =====================================================

install.packages("quantmod")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("TTR")
install.packages("forecast")
install.packages("patchwork")

# =====================================================
# STEP 2 : LOAD LIBRARIES
# =====================================================

library(quantmod)
library(ggplot2)
library(dplyr)
library(TTR)
library(forecast)
library(patchwork)

# =====================================================
# STEP 3 : STOCK NAME
# =====================================================

# EXAMPLES:
# TSL
# AAPL
# UBER
# MSFT
# RELIANCE.NS
# TCS.NS

stock_name <- "TSLA"

# =====================================================
# STEP 4 : DOWNLOAD STOCK DATA
# =====================================================

getSymbols(
  stock_name,
  src = "yahoo",
  from = "2022-01-01",
  auto.assign = TRUE
)

# =====================================================
# STEP 5 : CONVERT TO DATAFRAME
# =====================================================

stock_data <- data.frame(
  Date = index(get(stock_name)),
  coredata(get(stock_name))
)

# =====================================================
# STEP 6 : RENAME COLUMNS
# =====================================================

colnames(stock_data) <- c(
  "Date",
  "Open",
  "High",
  "Low",
  "Close",
  "Volume",
  "Adjusted"
)

# =====================================================
# STEP 7 : REMOVE MISSING VALUES
# =====================================================

stock_data <- na.omit(stock_data)

# =====================================================
# STEP 8 : DAILY RETURNS
# =====================================================

stock_data$Daily_Return <- c(
  NA,
  diff(stock_data$Close) /
    head(stock_data$Close, -1)
)

# =====================================================
# STEP 9 : MOVING AVERAGES
# =====================================================

stock_data$MA20 <- SMA(
  stock_data$Close,
  20
)

stock_data$MA50 <- SMA(
  stock_data$Close,
  50
)

# =====================================================
# STEP 10 : BUY / SELL SIGNALS
# =====================================================

stock_data$Signal <- ifelse(
  stock_data$MA20 > stock_data$MA50,
  "BUY",
  "SELL"
)

# =====================================================
# STEP 11 : KPI REPORT
# =====================================================

latest_close <- tail(
  stock_data$Close,
  1
)

highest_price <- max(
  stock_data$High
)

lowest_price <- min(
  stock_data$Low
)

avg_volume <- mean(
  stock_data$Volume
)

volatility <- sd(
  stock_data$Daily_Return,
  na.rm = TRUE
)

cat("\n========================")
cat("\n STOCK KPI REPORT")
cat("\n========================")

cat(
  "\nLatest Close Price : ",
  latest_close
)

cat(
  "\nHighest Price : ",
  highest_price
)

cat(
  "\nLowest Price : ",
  lowest_price
)

cat(
  "\nAverage Volume : ",
  round(avg_volume, 2)
)

cat(
  "\nVolatility : ",
  round(volatility, 5)
)

# =====================================================
# GRAPH 1 : CLOSING PRICE
# =====================================================

p1 <- ggplot(
  stock_data,
  aes(Date, Close)
) +
  geom_line(
    color = "red",
    linewidth = 1
  ) +
  ggtitle("Closing Price Trend") +
  theme_minimal()

# =====================================================
# GRAPH 2 : VOLUME
# =====================================================

p2 <- ggplot(
  stock_data,
  aes(Date, Volume)
) +
  geom_col(
    fill = "darkblue"
  ) +
  ggtitle("Trading Volume") +
  theme_minimal()

# =====================================================
# GRAPH 3 : HIGH VS LOW
# =====================================================

p3 <- ggplot(
  stock_data,
  aes(Date)
) +
  geom_line(
    aes(y = High,
        color = "High")
  ) +
  geom_line(
    aes(y = Low,
        color = "Low")
  ) +
  ggtitle("High vs Low Price") +
  theme_minimal()

# =====================================================
# GRAPH 4 : OPEN VS CLOSE
# =====================================================

p4 <- ggplot(
  stock_data,
  aes(Open, Close)
) +
  geom_point(
    color = "purple",
    size = 2
  ) +
  ggtitle("Open vs Close") +
  theme_minimal()

# =====================================================
# GRAPH 5 : MOVING AVERAGE ANALYSIS
# =====================================================

p5 <- ggplot(
  stock_data,
  aes(Date)
) +
  geom_line(
    aes(y = Close),
    color = "black"
  ) +
  geom_line(
    aes(y = MA20),
    color = "blue",
    linewidth = 1
  ) +
  geom_line(
    aes(y = MA50),
    color = "red",
    linewidth = 1
  ) +
  ggtitle("Moving Average Analysis") +
  theme_minimal()

# =====================================================
# GRAPH 6 : TREND ANALYSIS
# =====================================================

p6 <- ggplot(
  stock_data,
  aes(Date, Close)
) +
  geom_line(
    color = "darkgreen"
  ) +
  geom_smooth(
    method = "lm",
    color = "red",
    se = FALSE
  ) +
  ggtitle("Trend Analysis") +
  theme_minimal()

# =====================================================
# GRAPH 7 : RETURN HISTOGRAM
# =====================================================

p7 <- ggplot(
  stock_data,
  aes(Daily_Return)
) +
  geom_histogram(
    bins = 30,
    fill = "orange",
    color = "black"
  ) +
  ggtitle("Daily Return Histogram") +
  theme_minimal()

# =====================================================
# GRAPH 8 : BUY / SELL SIGNALS
# =====================================================

p8 <- ggplot(
  stock_data,
  aes(Date, Close)
) +
  geom_line(
    color = "black"
  ) +
  geom_point(
    aes(color = Signal),
    size = 2
  ) +
  ggtitle("Buy / Sell Signals") +
  theme_minimal()

# =====================================================
# STEP 12 : FORECASTING
# =====================================================

close_ts <- ts(stock_data$Close)

arima_model <- auto.arima(close_ts)

forecast_values <- forecast(
  arima_model,
  h = 30
)

png(
  "forecast_plot.png",
  width = 900,
  height = 500
)

plot(
  forecast_values,
  main = "30 Day Forecast"
)

dev.off()

# =====================================================
# STEP 13 : DASHBOARD
# =====================================================

dashboard <-
  (p1 | p2) /
  (p3 | p4) /
  (p5 | p6) /
  (p7 | p8)

print(dashboard)

# =====================================================
# STEP 14 : VIEW DATA
# =====================================================

View(stock_data)

# =====================================================
# STEP 15 : SAVE CSV
# =====================================================

write.csv(
  stock_data,
  "final_stock_analysis.csv",
  row.names = FALSE
)

# =====================================================
# END OF PROJECT
# =====================================================