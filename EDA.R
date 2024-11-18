library(tidymodels)
library(vroom)
library(patchwork)


# DATA --------------------------------------------------------------------

test <- vroom("test.csv")
train <- vroom("train.csv")


# EDA ---------------------------------------------------------------------

storeItem <- train %>%
      filter(store== ceiling(runif(1, min = 1, max =10)), item== ceiling(runif(1, min = 1, max =10)))

storeItem2 <- train %>%
  filter(store== ceiling(runif(1, min = 1, max =10)), item== ceiling(runif(1, min = 1, max =10)))

# TimeSeries Plot ---------------------------------------------------------

plot1 <- storeItem %>%
    ggplot(mapping=aes(x=date, y= sales)) +
    geom_line() +
    geom_smooth(se=FALSE)

plot4 <- storeItem2 %>%
  ggplot(mapping=aes(x=date, y= sales)) +
  geom_line() +
  geom_smooth(se=FALSE)

# Auto Correlation --------------------------------------------------------



plot2 <- forecast::ggAcf(storeItem %>%
                  pull(sales))

plot3 <- forecast::ggAcf(storeItem %>%
                  pull(sales), lag.max=2*365)

plot5 <- forecast::ggAcf(storeItem2 %>%
                  pull(sales))

plot6 <- forecast::ggAcf(storeItem2 %>%
                  pull(sales), lag.max=2*365)


# Patchwork ---------------------------------------------------------------

(plot1 + plot2 + plot3) / (plot4 + plot5 + plot6)
