library(tidymodels)
library(vroom)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions


# DATA --------------------------------------------------------------------

test <- vroom("test.csv")
train <- vroom("train.csv")


# Store Item Selection ----------------------------------------------------

storeItemTrain <- train %>%
  filter(store== 6, item== 8)

storeItemTest <- test %>%
  filter(store== 3, item== 7)




# CV Splits Plot ---------------------------------------------------------------


cv_split <- time_series_split(storeItemTrain, assess="3 months", cumulative = TRUE)

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


# Sarima Model ------------------------------------------------------------

prophet_model <- prophet_reg() %>%
set_engine(engine = "prophet") %>%
fit(sales ~ date, data = training(cv_split))


# Workflow ----------------------------------------------------------------


## Calibrate (tune) the models (find p,d,q,P,D,Q)
cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))

## Visualize results
p1 <- cv_results %>%
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = training(cv_split)) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## Now that you have calibrated (tuned) refit to whole dataset
fullfit <- cv_results %>%
  modeltime_refit(data=storeItemTrain)

# Predictions -------------------------------------------------------------

## Predict for all the observations in test1
p2 <- fullfit %>%
  modeltime_forecast(new_data = storeItemTest,
                     actual_data = storeItemTrain) %>%
  plot_modeltime_forecast(.interactive=FALSE)


# Store Item Selection ----------------------------------------------------

storeItemTrain <- train %>%
  filter(store== 5, item== 38)

storeItemTest <- test %>%
  filter(store== 2, item== 27)


# Recipe ------------------------------------------------------------------

my_recipe <- recipe(sales ~ ., data = train)  %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))


# CV Splits Plot ---------------------------------------------------------------


cv_split <- time_series_split(storeItemTrain, assess="3 months", cumulative = TRUE)

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


# Prophet Model ------------------------------------------------------------

prophet_model <- prophet_reg() %>%
set_engine(engine = "prophet") %>%
fit(sales ~ date, data = training(cv_split))


# Workflow ----------------------------------------------------------------



## Calibrate (tune) the models (find p,d,q,P,D,Q)
cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))

## Visualize results
p3 <- cv_results %>%
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = training(cv_split)) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## Now that you have calibrated (tuned) refit to whole dataset
fullfit <- cv_results %>%
  modeltime_refit(data=storeItemTrain)

# Predictions -------------------------------------------------------------

## Predict for all the observations in test1
p4 <- fullfit %>%
  modeltime_forecast(new_data = storeItemTest,
                     actual_data = storeItemTrain) %>%
  plot_modeltime_forecast(.interactive=FALSE)


# plot --------------------------------------------------------------------


plotly::subplot(p1,p3,p2,p4, nrows=2)

