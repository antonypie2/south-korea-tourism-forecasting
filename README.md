# south-korea-tourism-forecasting

## Overview
This project analyzes monthly short-term visitor arrivals from South Korea to Australia from January 1991 to December 2019.

The analysis explores trend and seasonal behaviour and compares statistical forecasting approaches.

## Methods
- Time-series exploration
- Seasonal and subseries analysis
- Autocorrelation analysis
- Guerrero/Box-Cox transformation
- Seasonal naïve benchmark with drift
- ARIMA modelling
- ETS modelling
- Residual diagnostics
- Ljung–Box testing
- Train/test forecast evaluation
- Forecast accuracy comparison

## Tools
R · fpp3 · tsibble · ggplot2 · readxl

## Data
The original analysis used monthly visitor-arrival data for South Korea to Australia. Do not commit a dataset unless its licence permits redistribution. Add the source, access date and licence/usage terms to `data/README.md`.

## Workflow
1. Explore the historical series and seasonal structure.
2. Assess autocorrelation and variance behaviour.
3. Apply a Box-Cox transformation where appropriate.
4. Establish a seasonal naïve benchmark with drift.
5. Fit ARIMA and ETS models.
6. Diagnose residuals.
7. Generate forecasts.
8. Evaluate models on a held-out two-year test period.

## Limitation
The historical series ends in 2019, so forecasts extending into 2020–2021 do not incorporate unprecedented structural shocks such as the COVID-19 pandemic.

## Academic Context
This repository is a portfolio adaptation of work completed during university coursework in forecasting and quantitative analysis.

## Author
Antony Wibgha
