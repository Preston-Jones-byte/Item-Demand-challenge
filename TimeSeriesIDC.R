library(tidymodels)
library(vroom)



# DATA --------------------------------------------------------------------

test <- vroom("test.csv")
train <- vroom("train.csv")



# Recipe ------------------------------------------------------------------

storeItem <- train %>%
  filter(store== ceiling(runif(1, min = 1, max = 10)), 
         item== ceiling(runif(1, min = 1, max = 50)))
storeItem



# Recipe ------------------------------------------------------------------


my_recipe <- recipe(sales ~ ., data = train)  %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))



# random forests ----------------------------------------------------------

my_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=50) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")

## Set Workflow
randf_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)

## Grid of values to tune over
grid_of_tuning_params <- grid_regular(mtry(range= c(1, 10)),
                                      min_n(),
                                      levels = 5) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(train, v = 5, repeats= 3)


## Run the CV
CV_results <- randf_wf %>%
          tune_grid(resamples=folds,
          grid=grid_of_tuning_params,
          metrics=metric_set(rmse) #Or leave metrics NULL



## Find Best Tuning Parameters
bestTune <- CV_results %>%
  show_best(metric="smape", n=1)

save(bestTune)