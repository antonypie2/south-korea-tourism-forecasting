### STUDENT DETAILS #####
# ECON1061 Final Assessment
# Name: Antony Wibgha
# Student ID: s4233062
# Allocated Country ID: A85375566F (South Korea)


#install.packages("fpp2")
#install.packages("fpp3")
library(fpp2)
library(fpp3)
library(readxl)

setwd("C:/Users/wibgh/OneDrive/6. Semester/Forecasting and quantitative analysis/Assesment 3")

#install.packages("readxl")
touristdata <- readxl::read_excel("Tourist_data-1.xlsx", skip = 1)
View(touristdata)
names(touristdata)

myts <- ts(touristdata[, "A85375566F"], frequency = 12, start = c(1991, 1))

myts <- myts %>%
  as_tsibble() %>%
  rename(tourist = value)


#PART A
# Question 1

myts %>%
  autoplot(tourist) +
  labs(title = "South Korea arrivals",
       y = "Arrivals",
       x = "Year")

myts %>%
  gg_season(tourist) +
  labs(title = "Seasonal pattern",
       y = "Arrivals",
       x = "Month")

myts %>%
  gg_subseries(tourist) +
  labs(title = "Monthly subseries",
       y = "Arrivals",
       x = "Year")

myts %>%
  ACF(tourist) %>%
  autoplot() +
  labs(title = "ACF of arrivals",
       y = "ACF",
       x = "Lag")


###### Question 2

lambda <- myts %>%
  features(tourist, features = guerrero) %>%
  pull(lambda_guerrero)

lambda

myts %>%
  autoplot(log(tourist)) +
  labs(title = "Log transformation",
       y = "log(arrivals)",
       x = "Year")

myts %>%
  autoplot(box_cox(tourist, lambda)) +
  labs(title = "Box-Cox transformation",
       y = "Box-Cox arrivals",
       x = "Year")


# Question 3

# Seasonal naive with drift is used as a simple benchmark for monthly seasonal data with trend.
fit_simple <- myts %>%
  model(snaivedr = SNAIVE(box_cox(tourist, lambda) ~ drift()))

fit_simple %>% report()


# Question 4

fit_simple %>%
  select(snaivedr) %>%
  gg_tsresiduals()

# Ljung-Box test for residual autocorrelation.
fit_simple %>%
  augment() %>%
  features(.innov, ljung_box, dof = 6, lag = 12)


# Question 5

fc_simple <- fit_simple %>% forecast(h = "2 years")
fc_simple

fc_simple %>%
  autoplot(myts) +
  labs(title = "SNAIVE drift forecast",
       y = "Arrivals",
       x = "Year")

mytstail <- tail(myts, 5 * 12)

fc_simple %>%
  autoplot(mytstail) +
  labs(title = "SNAIVE drift forecast: last 5 years",
       y = "Arrivals",
       x = "Year")


# PART B

#Question 6a

myts %>%
  gg_tsdisplay(box_cox(tourist, lambda), plot_type = "partial") +
  labs(title = "Box-Cox series")

myts %>% features(box_cox(tourist, lambda), unitroot_kpss)
myts %>% features(box_cox(tourist, lambda), unitroot_ndiffs)
myts %>% features(box_cox(tourist, lambda), unitroot_nsdiffs)

myts %>%
  autoplot(box_cox(tourist, lambda) %>% difference()) +
  labs(title = "First difference",
       y = "Differenced Box-Cox arrivals",
       x = "Year")

myts %>%
  gg_tsdisplay(box_cox(tourist, lambda) %>% difference(), plot_type = "partial") +
  labs(title = "First difference")

myts %>%
  autoplot(box_cox(tourist, lambda) %>% difference(lag = 12)) +
  labs(title = "Seasonal difference",
       y = "Seasonally differenced arrivals",
       x = "Year")

myts %>%
  gg_tsdisplay(box_cox(tourist, lambda) %>% difference(12), plot_type = "partial") +
  labs(title = "Seasonal difference")

myts %>%
  autoplot(box_cox(tourist, lambda) %>% difference(lag = 12) %>% difference()) +
  labs(title = "Seasonal and first difference",
       y = "Differenced Box-Cox arrivals",
       x = "Year")

myts %>%
  gg_tsdisplay(box_cox(tourist, lambda) %>% difference(12) %>% difference(),
               plot_type = "partial") +
  labs(title = "Seasonal and first difference")


# Question 6b

# Auto-ARIMA and manual candidates are compared using AICc.
fit_arima <- myts %>%
  model(arima = ARIMA(box_cox(tourist, lambda)))

fit_arima %>% report()

fit_manual <- myts %>%
  model(
    arima011_011 = ARIMA(box_cox(tourist, lambda) ~ pdq(0, 1, 1) + PDQ(0, 1, 1)),
    arima110_011 = ARIMA(box_cox(tourist, lambda) ~ pdq(1, 1, 0) + PDQ(0, 1, 1)),
    arima111_011 = ARIMA(box_cox(tourist, lambda) ~ pdq(1, 1, 1) + PDQ(0, 1, 1)),
    arima012_011 = ARIMA(box_cox(tourist, lambda) ~ pdq(0, 1, 2) + PDQ(0, 1, 1)),
    arima210_011 = ARIMA(box_cox(tourist, lambda) ~ pdq(2, 1, 0) + PDQ(0, 1, 1)),
    arima011_110 = ARIMA(box_cox(tourist, lambda) ~ pdq(0, 1, 1) + PDQ(1, 1, 0))
  )

fit_manual %>%
  glance() %>%
  arrange(AICc) %>%
  select(.model, AICc)


# Question 7

fit_ets <- myts %>%
  model(ets = ETS(box_cox(tourist, lambda) ~ error("A") + trend("A") + season("A")))

fit_ets %>% report()


# Question 8

fit_arima %>%
  select(arima) %>%
  gg_tsresiduals()

# Update dof according to the ARIMA model: dof = p + q + P + Q.
fit_arima %>%
  augment() %>%
  features(.innov, ljung_box, dof = 6, lag = 12)

fit_ets %>%
  select(ets) %>%
  gg_tsresiduals()

fit_ets %>%
  augment() %>%
  features(.innov, ljung_box, dof = 6, lag = 12)


# Question 9

fit_b <- myts %>%
  model(
    arima = ARIMA(box_cox(tourist, lambda)),
    ets   = ETS(box_cox(tourist, lambda) ~ error("A") + trend("A") + season("A"))
  )

fc_b <- fit_b %>% forecast(h = "2 years")
fc_b

fc_b %>%
  autoplot(myts) +
  labs(title = "ARIMA and ETS forecasts",
       y = "Arrivals",
       x = "Year")

mytstail <- tail(myts, 5 * 12)

fc_b %>%
  autoplot(mytstail) +
  labs(title = "ARIMA and ETS forecasts: last 5 years",
       y = "Arrivals",
       x = "Year")


# PART C 

# Question 10

train <- myts %>%
  slice(1:(n() - 24))

fit_train <- train %>%
  model(
    snaivedr = SNAIVE(box_cox(tourist, lambda) ~ drift()),
    arima    = ARIMA(box_cox(tourist, lambda)),
    ets      = ETS(box_cox(tourist, lambda) ~ error("A") + trend("A") + season("A"))
  )

fc_train <- fit_train %>% forecast(h = "2 years")
fc_train

fc_train %>%
  autoplot(myts) +
  labs(title = "Test-set forecasts",
       y = "Arrivals",
       x = "Year")

mytstail <- tail(myts, 5 * 12)

fc_train %>%
  autoplot(mytstail) +
  labs(title = "Test-set forecasts: last 5 years",
       y = "Arrivals",
       x = "Year")


# Question 11

fc_train %>%
  accuracy(myts) %>%
  arrange(MASE)
