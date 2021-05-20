# Load required packages
pkgs <- c("tidyverse", "lubridate", "knitr", "astsa", "timeDate", "xts", "tsbox", "TTR", "highcharter", "imputeTS", "forecast")
invisible(lapply(pkgs, library, character.only = T, warn.conflicts = F))

library(highcharter)
library(ggpubr)
# Set highcharter options
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

# Read daily 942 daily records
records <- read_csv("./Data/train.csv", col_types = cols("i", "i", "D", "d", "d", "f", "f", "f", "f"))

# Read information of each store
store <- read_csv("./Data/store.csv", col_types = cols("i", "c", "c", "d", "i", "i", "f", "i", "i", "c"))
head(store)

# ----------------------------------------------------------------------------------------------------------------------

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
  filter(!Store %in% idx_omitInt, !Store == 988)

# ----------------------------------------------------------------------------------------------------------------

# PROBLEM 2: less than 5% of stores open on Sunday
# SOLUTION: Delete  

# ID of stores that open on Sunday
id_openSunday = records_del %>%
  filter(DayOfWeek == 7 & Sales != 0) %>%
  .$Store %>%
  unique()

# Coerce the sales on Sunday to NA
records_del <- records_del %>%
  filter(!DayOfWeek == 7)

# ----------------------------------------------------------------------------------------------------------------

# PROBLEM 3: Sales equal to zero on holidays
# SOLUTION: imputation for sales in those days

# irregular holidays # 12-25 is a holiday but stores open on that day
records_del %>%
  filter(StateHoliday != 0) %>%
  .$Date %>%
  unique()

# coerce the the 0 sales value to NA
records_del[records_del$Sales == 0, "Sales"] = NA

# -----------------------------------------------------------------------------------------------------------------

# Transform train dataset into xts object
num_stores <- length(unique(records_del$Store))

# Spit the dataframe into a list of dataframe based on the id of store
records_del <- records_del %>%
  arrange(Date)

# Store index
store_id <- unique(records_del$Store)
records_lst <- split(records_del, f = records_del$Store)

# Transform each time series for each store to xts object 
records_xts <- lapply(records_lst, function(x) { as.xts(x$Sales, x$Date) })

# Take 1st Store for example
first_store <- records_xts[[which(store_id == 1)]]
ggplot_na_distribution(first_store, x_axis_labels = index(first_store))
imp <- na_kalman(first_store)
ggplot_na_imputations(first_store, imp, x_axis_labels = index(first_store))

# Using Kalman smoothing to impute missing values
records_xts_imp <- lapply(records_xts, na_kalman)

# ============================================================================================================

# Visualization for univariate variable: Sales

# Take 85th Store for example 
store_85 <- records_xts_imp[[which(store_id == 85)]]

# Time series plot 

# 1. Cycle over 2-weeks (12 days) period
# 2. Constant mean
# 3. Time-related variation # Box-Cox transformation
par(mfrow = c(2, 1), mex = 0.8, cex = 0.8)
plot(store_85, main = "85th Store")
plot(store_85["201301/201306"], main = "85th Store")

# Interactive time series plot
hchart(store_85)

# Normality # no
ggqqplot(store_85)
shapiro.test(coredata(store_85))

### Transformation 

# choose optimal lambda
BoxCox.lambda(store_85, method = "guerrero")

# Box-Cox transformation
store_85_bc = BoxCox(store_85, BoxCox.lambda(store_85, method = "guerrero"))

# Interactive time series plot 
hchart(store_85_bc)

# Normality # no
ggqqplot(store_85_bc)
shapiro.test(coredata(store_85_bc))

# ACF and PACF
acf2(store_85_bc, max.lag = 100, main = "Series: 85th Store")

# lag scatter plot 
lag1.plot(store_85_bc, max.lag = 12)

# Seasonal differencing (D=1, S=12)
store_bc_12d <- diff(store_85_bc, 12)

# Time series plot again
par(mfrow = c(3, 1), mex = 0.7, cex = 0.1)
plot(store_85["2013-01/2013-06"], main = "", yaxis.right = F)
plot(store_85_bc["2013-01/2013-06"], main = "", yaxis.right = F)
plot(store_bc_12d["2013-01/2013-06"], main = "", yaxis.right = F)
hchart(store_bc_12d)

# ACF and PACF
acf2(store_bc_12d, max.lag = 200, main = "Series: 85th Store")

# Differencing again (d = 1)
store_bc_d12d <- diff(store_bc_12d, 1)
hchart(store_bc_d12d)

# ACF and PACF
acf2(store_bc_d12d, max.lag = 200, main = "Series: 85th Store")

# ------------------------------------------------------------------------------------------------------

# Modeling
Auto.mod = auto.arima(store_85_bc, seasonal = T)
sarima(flu_bc, p = 1, d = 1, q = 1, P = 1, Q = 1, D = 1, S = 12, details = F)


