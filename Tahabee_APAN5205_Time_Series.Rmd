---
title: "R Notebook"
output: html_notebook
---
## Packages
```{r}
library(ggplot2);library(ggthemes);library(gridExtra)  # For plots 
library(quantmod);library(xts);library(zoo) # For using xts class objects
library(forecast) # Set of forecasting functions
library(fpp); library(fpp2) # Datasets from Forecasting text by Rob Hyndman
library(tseries) # for a statistical test
library(dplyr) # Data wrangling
```

## Import Data
```{r}
setwd('~/Documents/Applied Analytics Frameworks & Methods II/Assignments/')
goog = readRDS('goog.RDS')
```

## What type of data structure is goog?
```{r}
class(goog)
```

## What was google stock price for June, 2010?
```{r}
goog["2010-06"]
```

## How many months of data are included in this dataset?
```{r}
length(goog)
```

## With time-series data, the past is often a good predictor of the future. Let us see if this is true for our data. What is the correlation between google stock price and one-month lagged stock price? You can use lag() to obtain a one-month lag for google stock price. When computing correlation with cor(), be sure to set use='complete.obs'.
```{r}
goog_vect <- as.vector(goog)
goog_vect_lag <- lag(as.vector(goog))
cor(goog_vect, goog_vect_lag,use='complete.obs')
```
## In order to have access to a wider array of forecasting models, we will convert the data to a "ts" data type. Also, we will split the data into a train and test sample, using the train sample to estimate a model and the test sample to evaluate it. We will used data from Jan, 2007 to Dec, 2015 for the train sample and the rest for the test sample. The code below will convert goog to a “ts” object and split the data.
```{r}
google = ts(goog,start=c(2007,01),frequency=12)
train = window(google,start=c(2007,01),end=c(2015,12))
test = window(google,start=c(2016,01),end=c(2018,10))
```
##Autocorrelation examines correlation of a variable and its lagged values. Construct a plot of autocorrelations for train using ggAcf() from the forecast package. Which lag has the strongest autocorrelation?
```{r}
ggAcf(train)
```

## A very simple prediction, often the baseline in linear regression, is to use the average. Use the average to make a prediction for the stock price over the 34 months of the test sample. Let's call this average_model. What is the point forecast of the stock price for October 2018?
```{r}
average_model = meanf(train, h = 108)
average_model
window(average_model$mean,c(2010,10))
```

## Let us examine the accuracy of the above prediction from average_model on the train sample. 
## Specifically, what is the RMSE of the prediction in the train sample? Hint: Use accuracy() from library(forecast)
```{r}
accuracy(average_model)
```

## What is the RMSE of the average_model on the test sample?
```{r}
accuracy(average_model, google)
```

## Next, let us examine another simple prediction, one that assumes the future will be the same as the last observation. Let’s call this naive_model. Use naive_model to construct a forecast for stock price over the next 34 months of the test sample. What is the point forecast of the stock price for October 2018?
```{r}
naive_model = naive(train,h=108)
naive_model$mean
```

## What is the RMSE of the naive_model on the test sample?

```{r}
accuracy(naive_model,x=google)
```

## There are a number of exponential smoothing models that differ in how they handle errors, trend, and seasonality. Let us fit an exponential smoothing model. Call this ets_model.
```{r}
ets_model <- ets(train,model = 'AAA')
summary(ets_model)
```

## Do the residuals look like white noise? To answer this question, examine an Acf() or ggAcf() plot and the result of the Ljung-Box test.
```{r}
checkresiduals(ets_aaa)
```
## Use ets_model to construct a forecast for stock price over the next 34 months of the test sample. What is the point forecast of the stock price for October 2018?
```{r}
ets_aaa_forecast = forecast(ets_aaa,h=34)
ets_aaa_forecast
```
## What is the RMSE of the ets_model on the test sample?
```{r}
accuracy(ets_aaa_forecast,x = google)
```

## Now, let’s use an ARIMA model. Since, there are a large number of parameters with which to define the ARIMA model, let use the auto.arima() function to automatically determine the best parameters. Use the defaults for auto.arima(). For instance, if your dataset is called train, run auto.arima(train). Call this auto_arima_model. How many ordinary autoregressive lag variables have been used in auto_arima_model?
```{r}
auto_arima_model = auto.arima(train)
auto_arima_model
```

```{r}
checkresiduals(auto_arima_model)
```
## Use auto_arima_model to construct a forecast for stock price over the next 34 months of the test sample. What is the point forecast of the stock price for October 2018?
```{r}
auto_arima_forecast <- forecast(auto_arima_model, h = 34)
auto_arima_forecast
```
## What is the RMSE of auto_arima_model on the test sample?
```{r}
accuracy(auto_arima_forecast, google)
```


## Let us see if we can improve our ARIMA model by a variance stabilizing transformation. BoxCox.lambda() is a handy function for identifying the optimal value of lambda to stabilize variance. What is the optimal value of lambda?
```{r}
BoxCox.lambda(train)
```

## Rather than using auto.arima(), let us specify an ARIMA model using BoxCox lambda.  What is the AICc for arima_model?
```{r}
arima_model <- Arima(train,order = c(1,1,1),seasonal = c(3,1,0),lambda=BoxCox.lambda(train))
```

## Examine the results of Ljung-Box test (using the default of 24 lags) to see if the residuals resemble white noise.
```{r}
checkresiduals(arima_model)
```

## Use arima_model to construct a forecast for stock price over the next 34 months of the test sample. What is the point forecast of the stock price for October 2018?
```{r}
arima_forecast <- forecast(arima_model, h = 34)
arima_forecast
```

## What is the RMSE of arima_model on the test sample?

```{r}
accuracy(arima_forecast, google)
```

