# Load required packages
pkgs <- c("tidyverse", "lubridate", "knitr", "astsa", "timeDate", "xts", "tsbox", "TTR", "highcharter", "imputeTS", "forecast", "TSstudio")
invisible(lapply(pkgs, library, character.only = T, warn.conflicts = F))

library(highcharter)
library(htmlwidgets)
library(ggpubr)
# Set highcharter options
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

# Read daily 942 daily records
records <- read_csv("./Data/train.csv", col_types = cols("i", "i", "D", "d", "d", "f", "f", "f", "f"))
head(records)

# Read information of each store
store_info <- read_csv("./Data/store.csv", col_types = cols("i", "c", "c", "d", "i", "i", "f", "i", "i", "c"))
head(store_info)

# ================================================================================================================

############ Preprocessing ###########

# PROBLEM 1: Some stores do not have records from 2014-06-30 to 2014-12-31
# SOLUTION: try to delete these store during the analysis

# Find out the id of stores do no have records among this period
idx_omitInt = records %>%
  group_by(Store) %>%
  summarise(record_cnts = n()) %>%
  arrange(desc(record_cnts)) %>%
  filter(record_cnts == 758) %>%
  .$Store

# Delete them from records
records_del <- records %>%
  filter(!Store %in% idx_omitInt, !Store == 988) # and 988

# ----------------------------------------------------------------------------------------------------------------

# PROBLEM 2: less than 5% of stores open on Sunday
# SOLUTION: Delete  

# ID of stores that open on Sunday
id_openSunday = records_del %>%
  filter(DayOfWeek == 7 & Open == 1) %>%
  .$Store %>%
  unique()

# Coerce the sales on Sunday to NA
records_del <- records_del %>%
  filter(!DayOfWeek == 7)

# ----------------------------------------------------------------------------------------------------------------

# PROBLEM 3: Sales equal to zero on holidays
# SOLUTION: imputation for sales in those days

# irregular holidays # 12-25 is a holiday but stores open on that day
date_stateHoliday <- records_del %>%
  filter(StateHoliday != 0) %>%
  .$Date %>%
  unique()

# coerce the the 0 sales value to NA
records_del[records_del$Sales == 0, "Sales"] = NA
records_del[records_del$Customers == 0, "Customers"] = NA
records_del$Promo <- 
  as.integer(as.character(records_del$Promo))
records_del$StateHoliday <- 
  as.integer(as.character(factor(records_del$StateHoliday, labels = c(0, 1, 2, 3))))

# -----------------------------------------------------------------------------------------------------------------

# records_del:

# 1. Delete Sunday records
# 2. Delele the records in omitted stores
# 3. Transform the sales value on Holidays to NA


# Transform records_del dataset into xts object
num_stores <- length(unique(records_del$Store))

# Spit the dataframe into a list of dataframe based on the id of store
records_del <- records_del %>%
  arrange(Date)

# Store index
idx_store <- unique(records_del$Store)
records_lst <- split(records_del, f = records_del$Store)

# Transform each time series for each store to xts object 
records_xts <- 
  lapply(
    records_lst, 
    function(x) { as.xts(x[, c("Sales", "Customers", "Promo", "StateHoliday")], x$Date) }
    )

# Take 1st Store for example
first_store <- records_xts[[which(store_id == 1)]]
ggplot_na_distribution(first_store$Sales, x_axis_labels = index(first_store))
imp <- na_kalman(first_store$Sales)
ggplot_na_imputations(first_store$Sales, imp, x_axis_labels = index(first_store))
ggsave("Picture/imputation.png", width = 8, height = 6, dpi = 300)

# Using Kalman smoothing to impute missing values # Both Sales and Customers
records_xts_imp <- lapply(records_xts, na_kalman)

# ============================================================================================================

# Vis for "Univariate variable": Sales

# Take 85th Store for example
store <- records_xts_imp[[which(store_id == 85)]]

# Time Series Plot

# 1. Cycle over 2-weeks (12 days) period
# 2. Constant mean
# 3. Time-related variation # Box-Cox transformation
# 4. Dec. 21, 22, 23, 24
png("Picture/Sales.png", width = 800, height = 600)
par(mfrow = c(2, 1), mex = 0.8, cex = 0.8)
plot(store["201301/201506"][, "Sales"], main = "Sales at 85th store", yaxis.right = F)
plot(store["201301/201306"][, "Sales"], main = "", yaxis.right = F)
dev.off()


# MA smoothing
png("Picture/MA.png", width = 800, height = 600)
par(mfrow = c(2, 1), mex = 0.8, cex = 0.8)
plot(store["201301/201506"][, "Sales"], main = "Sales at 85th store", yaxis.right = F)
plot(SMA(store["201301/201506"][, "Sales"], n = 6), on = 1, main = "13-units MA smoothing", col = "black", yaxis.right = F)
dev.off()

# Interactive time series plot
hc_ts <- hchart(store[, "Sales"])
saveWidget(hc_ts, file = file.path(getwd(), 'Picture/widget/', 'ts_widget.html'))

# Normality # Far from normal distribution 
ggqqplot(coredata(store)[, "Sales"], title = "Q-Q Plot for Sales")

# choose optimal lambda
BoxCox.lambda(store$Sales, method = "guerrero")

# Box-Cox transformation
store$"Sales_BC" <- BoxCox(store[, "Sales"], BoxCox.lambda(store[, "Sales"], "guerrero"))

# Log transformation
store$"Sales_log" <- log(store$Sales)

# Visualization for the original data and data obtained from Box-Cox transofrmaiton
png("Picture/trans_ts.png", width = 800, height = 600)
par(mfrow = c(3, 1), mex = 0.8, cex = 0.8)
plot(store["201301/201506"][, "Sales"], main = "Sales", yaxis.right = F)
plot(store["201301/201506"][, "Sales_log"], main = "Sales with log trans", yaxis.right = F)
plot(store["201301/201506"][, "Sales_BC"], main = "Sales with Box-Cox trans", yaxis.right = F)
dev.off()

# Normality 
store %>%
  coredata() %>%
  data.frame() %>%
  mutate("Sales" = scale(Sales), "Sales_BC" = scale(Sales_BC), "Sales_log" = scale(Sales_log)) %>%
  tidyr::pivot_longer(cols = c("Sales", "Sales_log", "Sales_BC")) %>%
  ggqqplot(x = "value", facet.by = "name")

ggsave("Picture/QQplot.png", width = 8, height = 6, dpi = 300)


# ACF and PACF
png("Picture/acf2_log.png", width = 800, height = 600)
acf2(store[, "Sales_log"],  max.lag = 48, main = "Series: log(Sales)")
dev.off()


# Seasonal differencing (D=1, S=12)
store$Sales_12d <- diff(store[, "Sales_log"], 12)

# Time series plot
png("Picture/seasonal_diff.png", width = 800, height = 600)
par(mfrow = c(3, 1), mex = 0.7, cex = 0.1)
plot(store["2014-01/2014-06"][, "Sales"], main = "Sales", yaxis.right = F)
plot(store["2014-01/2014-06"][, "Sales_log"], main = "Sales with log trans", yaxis.right = F)
plot(store["2014-01/2014-06"][, "Sales_12d"], main = "12-period differencing with log sales", yaxis.right = F)
dev.off()

# Interactive time series plot
hc_12dts <- hchart(store[, "Sales_12d"])
saveWidget(hc_12dts, file = file.path(getwd(), 'Picture/widget2/', 'ts_widget.html'))

# ACF and PACF # Still possess some trend
png("Picture/acf2_12d.png", width = 800, height = 600)
acf2(store[, "Sales_12d"], max.lag = 48, main = "Series: 12-period differncing with log(sales)")
dev.off()

# differencing again (d = 1)
store$"Sales_12dd" <- diff(store[, "Sales_12d"], 1)

# Time series plot again
png("Picture/diff_ts.png", width = 800, height = 600)
par(mfrow = c(2, 1), mex = 0.7, cex = 0.1)
plot(store["2014-01/2014-06"][, "Sales_12d"], main = "12-period differencing with log(sales)", yaxis.right = F)
plot(store["2014-01/2014-06"][, "Sales_12dd"], main = "12-period plus 1-peroid differencing with log(sales)", yaxis.right = F)
dev.off()

# Interactive time series plot
hc_12ddts <- hchart(store[, "Sales_12dd"])
saveWidget(hc_12dts, file = file.path(getwd(), 'Picture/widget3/', 'ts_widget.html'))

# ACF and PACF # Pattern seems to disappear
png("Picture/acf2_12dd.png", width = 800, height = 600)
acf2(store[, "Sales_12dd"], max.lag = 48, main = "Series: 12-period plus 1-period differncing with log(sales)")
dev.off()

# Ljung-Box test of the differenced series
Box.test(store[, "Sales_12dd"], type = "Ljung", lag = 1)


# Seasonal decomposition 

# transform from xts into ts object
store_ts <- ts(as.vector(coredata(records_xts_imp[[which(store_id == 82)]][, "Sales"])), frequency = 12)

# STL decomposition 
store_ts %>%
  mstl(robust=TRUE) %>%
  autoplot()

# Seasonal effect changes over time


# ==================================================================================================================================

### Modeling

library(TSstudio)

# Train data
split_store <- ts_split(store_ts, sample.out = 12)
store_train <- split_store$train
store_test <- split_store$test

# Auto Arima fit to obtained the optimal parameters based on information criteria # (0,1,1)(0,1,2)
auto_arima_fit <- auto.arima(store_train, stepwise = F, d = 1, parallel = T, num.cores = 4, D = 1, lambda = 0)

# SARIMA implemented by using Arima() in forecast package # -303.35 # -454.72
sarima_fit_auto <- 
  Arima(store_train, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 2), period = 12), include.constant = T, lambda = 0) 
sarima_fit_man <- 
  Arima(store_train, order = c(1, 1, 1), seasonal = list(order = c(2, 1, 1), period = 12), include.constant = T, lambda = 0) 

# Predictions
fc_auto <- forecast(sarima_fit_auto, h = 12)
fc_man <- forecast(sarima_fit_man, h = 12)

# Visualization the prediction 
g1 <- autoplot(window(store_train, start = 55)) +
  autolayer(fc_auto) +
  ggtitle("Forecasts from AutoSARIMA(0,1,1)(0,1,2)[12]") +
  xlab("Time") + 
  ylab("Sales") +
  guides(colour = guide_legend(title="Forecast")) 

g2 <- autoplot(window(store_train, start = 55)) +
  autolayer(fc_man) +
  ggtitle("Forecasts from SARIMA(1,1,1)(2,1,1)[12]") +
  xlab("Time") + 
  ylab("Sales") +
  guides(colour = guide_legend(title="Forecast")) 

gridExtra::grid.arrange(g1, g2)


# SARIMA implemented by sarima() in asta package
sarima(
  log(store_train), p = 0, d = 1, q = 1, P = 0, Q = 2, D = 1, S = 12 # -0.3820529
)

sarima(
  log(store_train), p = 1, d = 1, q = 1, P = 2, Q = 1, D = 1, S = 12 # -0.5726961 
)


# ---------------------------------------------------------------------------------------------------------

# Dynamic regression model with SARIMA noise

# Training set
n_train <- length(store_train)
X_tr <- records_xts_imp[[which(store_id == 82)]][1:n_train, "Promo"] %>% coredata()
X_tt <- records_xts_imp[[which(store_id == 82)]][(n_train+1):808, "Promo"] %>% coredata()

# Diagonistc
sarima(
  log(store_train), p = 0, d = 1, q = 1, P = 0, Q = 2, D = 1, S = 12, xreg = X_tr # -0.9773444
)
sarima(
  log(store_train), p = 1, d = 1, q = 1, P = 1, Q = 1, D = 1, S = 12, xreg = X_tr # -0.9818409
)

# Build Dynamic regression model with Promo variable
reg_sarima_fit_auto <- 
  auto.arima(
    store_train, d = 1, D = 1, stepwise = F, parallel = T, num.cores = 4,
    xreg = X_tr,
    lambda = 0
    )

reg_sarima_fit_man <- 
  Arima(
    store_train, 
    order = c(1, 1, 1), 
    seasonal = list(order = c(1, 1, 1), period = 12), 
    include.constant = T, 
    xreg = X_tr,
    lambda = 0
  )

# Predictions
fc_auto <- forecast(reg_sarima_fit_auto, h = 12, xreg = X_tt)
fc_man <- forecast(reg_sarima_fit_man, h = 12, xreg = X_tt)

# Visualization the prediction 
g11 <- autoplot(window(store_train, start = 55)) +
  autolayer(fc_auto) +
  ggtitle("Forecasts from Dynamic regression with autoSARIMA error") +
  xlab("Year") + 
  ylab("Sales") +
  guides(colour = guide_legend(title="Forecast")) 

g22 <- autoplot(window(store_train, start = 55)) +
  autolayer(fc_man) +
  ggtitle("Forecasts from Dynamic regression with SARIMA error") +
  xlab("Year") + 
  ylab("Sales") +
  guides(colour = guide_legend(title="Forecast")) 

gridExtra::grid.arrange(g11, g22)

# ----------------------------------------------------------------------------------------------------------------------------------

# Fourer series with ARIMA 

## Choose the parameters
plots <- list()
for (i in seq(6)) {
  fit <- 
    auto.arima(store_train, xreg = cbind(X_tr, fourier(store_train, K = i)), lambda = 0, seasonal = F, stepwise = F, parallel = T, num.cores = 4)
  
  plots[[i]] <- 
    autoplot(window(store_train, start = 55)) +
    autolayer(
      forecast(fit, xreg = cbind(X_tt, fourier(store_train, K = i, h = 12)))
      ) +
    xlab(paste("K=", i,"   AICC=",round(fit[["aicc"]],2))) +
    ylab("")
}

gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3
  )


# K = 6 is optimal

# Fit fourier (K=6) dynamic regression with ARIMA error # ARIMA(0,1,5)
reg_arima_fourier <- 
  auto.arima(store_train, xreg = cbind(X_tr, fourier(store_train, K = 6)), lambda = 0, seasonal = F, stepwise = F, parallel = T, num.cores = 4)

# Forecast
fc_f <- forecast(reg_arima_fourier, xreg = cbind(X_tt, fourier(store_train, K = 6, h = 12)))

# Vis
autoplot(window(store_train, start = 55)) +
  autolayer(fc_f) +
  ggtitle("Forecasts from Dynamic regression with fourier predictors and ARIMA error") +
  xlab("Year") + 
  ylab("Sales") +
  guides(colour = guide_legend(title="Forecast")) 


# ------------------------------------------------------------------------------------------------------------------------

### Compare three models: SARIMA, dynamic regression model with SARIMA error, fourier regression with ARIMA error

# AIC
AIC(sarima_fit_auto, sarima_fit_man, reg_sarima_fit_auto, reg_sarima_fit_man, reg_arima_fourier)

# Accuracy
accuracy(fc_f, store_test)
accuracy(fc_auto, store_test)
accuracy(fc_man, store_test)

DLR_SARIMAerr_mod <- function(x, h, xreg, newxreg) {
  forecast(
    Arima(
      x, 
      order = c(1, 1, 1), 
      seasonal = list(order = c(1, 1, 1), period = 12), 
      include.constant = T, 
      xreg = xreg,
      lambda = 0
    ),
    h = h, 
    xreg = newxreg 
  )
}

SARIMA_mod <- function(x, h, xreg, newxreg) {
  forecast(
    Arima(
      x, 
      order = c(1, 1, 1), 
      seasonal = list(order = c(2, 1, 1), period = 12), 
      include.constant = T, 
      lambda = 0
      ) ,
    h = h
  )
}

DLR_fourier_ARIMAerr_mod <- function(x, h, xreg, newxreg) {
  forecast(
    Arima(
      x, order = c(0, 1, 5),
      xreg = cbind(xreg, fourier(x, K = 6)), 
      lambda = 0, 
      seasonal = F
    ),
    h = h,
    xreg = cbind(newxreg, fourier(x, K = 6, h = h))
  )
}

# TS cv
SARIMA_cv <- tsCV(store_train, SARIMA_mod, xreg = X_tr, h = 1)
DLR_SARIMAerr_cv <- tsCV(store_train, DLR_SARIMAerr_mod, xreg = X_tr, h = 1)
DLR_fourier_ARIMAerr_cv <- tsCV(store_train, DLR_fourier_ARIMAerr_mod, xreg = X_tr, h = 1)
