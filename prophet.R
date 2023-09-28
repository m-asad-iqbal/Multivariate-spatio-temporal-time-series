######################
# New Approach
######################
# Loading reqiured packages.

install.packages("prophet")

library(dplyr)
library(tsibble)
library(ggplot2)
library(openxlsx)
library(ggthemes)
library(reshape2)
library(fable)
library(lubridate)
library(tidyverse)
library(forecast)
library(tseries)
library(fpp2)
library(writexl)
library(geofacet)
library(prophet)


data = main_data
data1<- data

### Geom Line ###

plot1 <- data1 %>% 
  ggplot(aes(x = ds, y = pred, color = product)) +
  geom_line() +
  facet_wrap(~`level2code`, scales = "free", ncol = 10) +
  theme(legend.position = "top") 

plot2 <- data1 %>% 
  ggplot(aes(x = ds, y = pred, color = level2code)) +
  geom_line() +
  facet_wrap(~`product`, scales = "free", ncol = 10) +
  theme(legend.position = "top") 

### Geom Point ### 

# Combined Pattern of the Product Garri

modeldata1 <- data1 %>%
  ggplot(aes(x = ds, y = pred, color = product)) +
  geom_smooth() +
  geom_point() +
  facet_wrap(~`product`, scales = "free") +
  theme(legend.position = "none")

# Patterns divided into Area Codes

modeldata2 <- data1 %>%
  ggplot(aes(x = ds, y = pred, color = level2code)) +
  geom_smooth() +
  geom_point() +
  facet_wrap(~`level2code`, scales = "free") +
  theme(legend.position = "none")

###### Forecasting ######

### Model Building ###

as.Date(data1$ds)
data1$date <- as.Date(data1$date, format = "%d/%m/%Y")
# Seting hte format of the date in the right order.

model_data <- data1 %>%
  dplyr::select(ds, pred, level2code) 

key <- model_data %>%
  select(level2code) %>%
  distinct() %>%
  mutate(name = c(1:78)) 
# Building a key for every seperate area. In our case we have 78.

model_results <- data.frame() # creating a empty model frame to store the results later.

for (i in 1:78){
  model <- model_data %>%
    filter(level2code == key$level2code[i]) %>%
    select(ds, y = pred) %>%
    prophet()
  assign(paste(key$name[i], "model", sep = "_"), model)

  Predicted_values <- make_future_dataframe(model, periods = 92)

  results <- predict(model, Predicted_values) %>%
    select(ds, yhat_lower, yhat_upper, yhat) %>%
    mutate(level2code = key$level2code[i],
           ds = as.Date(ds)) %>%
    inner_join(model_data, by = c("level2code" = "level2code", "ds" = "ds"))
  model_results <- rbind(model_results, results)
}

model_results %>% 
  ggplot(aes(x = ds, y = pred, color = level2code)) +
  geom_line(color = "black") +
  geom_line(aes(x = ds, y = yhat, color = level2code)) +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper, fill = level2code), alpha = .1) +
  facet_wrap(~level2code, scales = "free") +
  theme(legend.position = "none") +
  labs(title = "Foreacsting with Prophet",
       subtitle = "Forecasting Price for 78 Locations")

### Trend Plot of predicted vs Actual ###

Trend_plot <- model_results %>%
  ggplot(aes(x = yhat, y = pred, color = level2code)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(~level2code, scales = "free") +
  theme(legend.position = "none")

### Residual Plot ###

# Creating a residual plot to see the distribution of residuals along every area we have.

Residual_plot <- model_results %>%
  select(level2code, ds, predicted = yhat, actual = pred) %>%
  mutate(residual = predicted - actual) %>%
  ggplot(aes(x = residual,fill = residual > 0)) +
  geom_density(color = "white", alpha = .5) +
  facet_wrap(~level2code, scales = "free", ncol = 15) +
  theme(legend.position = "none")

### Area  Forecast ###

Area_forecast <- data1 %>%
  select(level2code, ds , y = pred) %>%
  group_by(level2code) %>%
  mutate(ds = paste(ds, "01", "01", sep = "-") %>% as.Date()) %>%
  group_by(level2code) %>%
  do(model = prophet(.), 
     Predicted_values = make_future_dataframe(model,92),
     results = predict(model, Predicted_values))

# 92 represents the days to be forecasted.
# Here we have set the weekly,daily and yearly seasonality to auto, To let the model deal with this predictions.
options(max.print=10000)
Area_forecast$results[[1]] 

# Change value from 1 to 78 to see the trend and the predicted values for every locations.
# Data provided has 214 rows, the new data has 306 rows up till Jan 2022.
# In our R code and data, yhat is forecast/predicted values, yhat_lower is prediction at lower intervals and yhat_upper is prediction at upper intervals.

Area_forecast %>%
  unnest(results) %>%
  select(level2code, ds, yhat, yhat_upper, yhat_lower) %>%
  ggplot(aes(x = ds, y = yhat, color = level2code)) +
  geom_ribbon(aes(ymax = yhat_upper, ymin = yhat_lower, fill = level2code), alpha = .2) +
  geom_smooth() +
  geom_line(color = "black") +
  facet_wrap(~ level2code, scales = "free") +
  theme(legend.position = "none")


### Simple Forecast ###

future_values <- Predicted_values
forecast <- predict(model, Predicted_values)
forecast_data <- forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]

### Data for Future ###

# If we wan to see all the new data points generated by our prediction model we can use this command here.
foreacst_data

# If we want to analyze the actual vs predicted data we can use this plot.
dyplot.prophet(model, forecast_data)


