---
title: "R Notebook"
output: html_notebook
---
```{r}
library(ggplot2);library(ggthemes);library(gridExtra)  # For plots 
library(quantmod);library(xts);library(zoo) # For using xts class objects
library(forecast) # Set of forecasting functions
library(fpp); library(fpp2) # Datasets from Forecasting text by Rob Hyndman
library(tseries) # for a statistical test
library(dplyr) # Data wrangling
```

```{r}
train = window(ausbeer, end=c(1999,4))
test = window(ausbeer, start=c(2000,1))
```

```{r}
## Average Method
average_model = meanf(train,h = 42)

average_model

average_model$mean

window(average_model$mean,c(2010,02))

accuracy(average_model,x = ausbeer)

autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(test)
```
```{r}
## Naive Method
naive_model = naive(train,h=42)

naive_model$mean

last(train)

accuracy(naive_model,x=ausbeer)

autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(naive_model,PI=F,size=1.1, series='Naive Model')+
  autolayer(test)
  ```
  
  ```{r}
# Seasonal Naive Model
seasonal_naive_model = snaive(train,h=42)

seasonal_naive_model

seasonal_naive_model$mean

accuracy(seasonal_naive_model,x = ausbeer)

autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(naive_model,PI=F,size=1.1, series='Naive Model')+
  autolayer(seasonal_naive_model,PI=F,size=1.1,series='Seasonal Naive Model')+
  autolayer(test)
```

```{r}
# Drift Model
drift_model = rwf(train,h=42,drift = T)

drift_model$mean

accuracy(drift_model,x = ausbeer)

autoplot(train)+
  autolayer(average_model,PI = F,size=1.1,series = 'Average Model')+
  autolayer(naive_model,PI=F,size=1.1, series='Naive Model')+
  autolayer(seasonal_naive_model,PI=F,size=1.1,series='Seasonal Naive Model')+
  autolayer(drift_model,PI=F,size=1.1,series='Drift Model')+
  autolayer(test)
```
```{r}
# Simple Exponential Smoothing Model
ses_model = ses(train,h = 42)

ses_model$mean

accuracy(ses_model,x = ausbeer) 

autoplot(train)+
  autolayer(ses_model,series = "Simple Exponential Smoothing",PI = F, size=1.1)+
  autolayer(test)
  ```
  
  ```{r}
# Holt Method
holt_model = holt(train,h=42)

holt_model$mean

accuracy(holt_model,x=ausbeer)

autoplot(train)+
  autolayer(ses_model,series = "Simple Exponential Smoothing",PI = F, size=1.1)+
  autolayer(holt_model,series="Holt's Method",PI=F,size=1.1)+
  autolayer(test)
```
```{r}
# Holt Method with Damping
holt_damped_model = holt(train,h=42,damped = T)

holt_damped_model$mean

accuracy(holt_damped_model,x=ausbeer)

autoplot(train)+
  autolayer(ses_model,series = "Simple Exponential Smoothing",PI = F, size=1.1)+
  autolayer(holt_model,series="Holt's Method",PI=F,size=1.1)+
  autolayer(holt_damped_model,series="Holt's Method with Damping",PI=F,size=1.1)+
  autolayer(test)
```


  



  

