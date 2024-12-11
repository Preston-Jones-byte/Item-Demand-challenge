library(tidymodels)
library(vroom)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions


# DATA --------------------------------------------------------------------

test <- vroom("test.csv")
train <- vroom("train.csv")


# Store Item Selection 1 ----------------------------------------------------

storeItemTrain <- train %>%
      filter(store== 6, item== 8)

storeItemTest <- test %>%
  filter(store== 3, item== 7)


# Recipe ------------------------------------------------------------------

my_recipe <- recipe(sales ~ ., data = storeItemTrain)  %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))


# CV Splits Plot ---------------------------------------------------------------


cv_split <- time_series_split(storeItemTrain, assess="3 months", cumulative = TRUE)

cv_split %>%
tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


# Sarima Model ------------------------------------------------------------

arima_model <- arima_reg(seasonal_period = 365,
                         non_seasonal_ar =5, # default max p to tune
                         non_seasonal_ma =5, # default max q to tune
                         seasonal_ar =2, # default max P to tune
                         seasonal_ma =2, #default max Q to tune
                         non_seasonal_differences =2, # default max d to tune
                         seasonal_differences=2)  %>%
                         set_engine("auto_arima")


# Workflow 1 ----------------------------------------------------------------

arima_wf <- workflow()        %>%
  add_recipe(my_recipe)       %>%
  add_model(arima_model)      %>%
  fit(data=training(cv_split))

## Calibrate (tune) the models (find p,d,q,P,D,Q)
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

## Visualize results
p1 <- cv_results %>%
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = training(cv_split)) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## Now that you have calibrated (tuned) refit to whole dataset
fullfit <- cv_results %>%
           modeltime_refit(data=storeItemTrain)

# Predictions 1 -------------------------------------------------------------

## Predict for all the observations in test1
p2 <- fullfit %>%
    modeltime_forecast(new_data = storeItemTest,
                   actual_data = storeItemTrain) %>%
    plot_modeltime_forecast(.interactive=FALSE)


# Store Item Selection 2 ----------------------------------------------------

storeItemTrain <- train %>%
  filter(store== 5, item== 38)

storeItemTest <- test %>%
  filter(store== 2, item== 27)


# Recipe ------------------------------------------------------------------

my_recipe <- recipe(sales ~ ., data = storeItemTrain)  %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))


# CV Splits Plot ---------------------------------------------------------------


cv_split <- time_series_split(storeItemTrain, assess="3 months", cumulative = TRUE)

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


# Sarima Model 2 ------------------------------------------------------------

arima_model <- arima_reg(seasonal_period = 365,
                         non_seasonal_ar =5, # default max p to tune
                         non_seasonal_ma =5, # default max q to tune
                         seasonal_ar =2, # default max P to tune
                         seasonal_ma =2, #default max Q to tune
                         non_seasonal_differences =2, # default max d to tune
                         seasonal_differences=2)  %>%
  set_engine("auto_arima")


# Workflow 2 ----------------------------------------------------------------

arima_wf <- workflow()        %>%
  add_recipe(my_recipe)       %>%
  add_model(arima_model)      %>%
  fit(data=training(cv_split))

## Calibrate (tune) the models (find p,d,q,P,D,Q)
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

## Visualize results
p3 <- cv_results %>%
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = training(cv_split)) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## Now that you have calibrated (tuned) refit to whole dataset
fullfit <- cv_results %>%
  modeltime_refit(data=storeItemTrain)

# Predictions 2 -------------------------------------------------------------

## Predict for all the observations in test1
p4 <- fullfit %>%
  modeltime_forecast(new_data = storeItemTest,
                     actual_data = storeItemTrain) %>%
  plot_modeltime_forecast(.interactive=FALSE)


# plot final --------------------------------------------------------------------


plotly::subplot(p1,p3,p2,p4, nrows=2)