# Time Series Analysis - NYC Births
# Author: Pol Serra Pozas
# Objective: Estimate trend, seasonality and forecast monthly births in NYC.

# 1. Load data -------------------------------------------------------------

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

# Remove first 12 observations to keep data from 1947 to 1959
births_ny <- births[-(1:12)]

time <- 1:length(births_ny)


# 2. Plot original time series --------------------------------------------

plot(
  time, births_ny,
  type = "l",
  xlim = c(1, length(births_ny)),
  col = "red",
  main = "Monthly Births in New York City",
  xlab = "Year",
  ylab = "Births (thousands)",
  axes = FALSE
)

axis(1, at = seq(1, 157, by = 12), labels = 1947:1960)
axis(2)
abline(v = seq(1, 157, by = 12), col = "lightgray")


# 3. Estimate linear trend -------------------------------------------------

trend_model <- lm(births_ny ~ time)

intercept <- coef(trend_model)[1]
slope <- coef(trend_model)[2]

abline(intercept, slope, col = "blue", lwd = 2)


# 4. Remove trend ----------------------------------------------------------

detrended_series <- births_ny - (intercept + slope * time)

plot(
  time, detrended_series,
  type = "l",
  xlim = c(1, length(births_ny)),
  col = "red",
  main = "Detrended Series",
  xlab = "Year",
  ylab = "Births minus trend",
  axes = FALSE
)

axis(1, at = seq(1, 157, by = 12), labels = 1947:1960)
axis(2)
abline(v = seq(1, 157, by = 12), col = "lightgray")
abline(h = 0, col = "blue", lwd = 2)


# 5. Estimate seasonal component ------------------------------------------

detrended_matrix <- matrix(detrended_series, ncol = 12, byrow = TRUE)
dimnames(detrended_matrix) <- list(1947:1959, month.abb)

seasonal_coefficients <- apply(detrended_matrix, 2, mean)

print(round(seasonal_coefficients, 3))


# 6. Build additive model --------------------------------------------------

seasonal_component <- rep(seasonal_coefficients, 13)
trend_component <- intercept + slope * time

fitted_values <- trend_component + seasonal_component


# 7. Plot original series and fitted model --------------------------------

plot(
  time, births_ny,
  type = "l",
  xlim = c(1, length(births_ny)),
  col = "black",
  main = "NYC Births: Original Series vs Additive Model",
  xlab = "Year",
  ylab = "Births (thousands)",
  axes = FALSE
)

axis(1, at = seq(1, 157, by = 12), labels = 1947:1960)
axis(2)

lines(time, fitted_values, col = "red", lwd = 2)

legend(
  "topleft",
  legend = c("Original series", "Additive model"),
  col = c("black", "red"),
  lty = 1,
  lwd = c(1, 2),
  cex = 0.9
)


# 8. Forecast future value -------------------------------------------------

# Forecast for February 1963
future_time <- 156 + 3 * 12 + 2
forecast_feb_1963 <- intercept + slope * future_time + seasonal_coefficients["Feb"]

print(round(forecast_feb_1963, 3))
