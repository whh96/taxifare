library(tidyverse)

taxi <- read.csv(file.choose())

head(taxi)

taxi <- taxi %>%
  rename( long = pickup_longitude, lat = pickup_latitude) %>%
  filter(fare_amount > 0 | tip_amount > 0) %>%
  mutate(total= log(fare_amount + tip_amount))

taxi <- taxi %>%
  filter(between(lat, 40.70, 40.83) & 
         between(long, -74.025, -73.93))

library(ggmap)
library(viridis)

manhattan <- readRDS(file.choose())

ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') + 
  geom_bin2d(data = taxi, aes(x = long,y = lat), bins = 60, alpha = 0.6) +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Journeys')

library(leaflet)
library(tree)

fitted_tree <- tree(total ~ lat + long, data = taxi)

plot(fitted_tree)
text(fitted_tree)

library(lubridate)

taxi <- taxi %>%
  mutate(
    hour = hour(pickup_datetime),
    wday = wday(pickup_datetime), label = TRUE, 
    month = month(pickup_datetime), label = TRUE
  )
head(taxi)

fitted_tree <- tree(total ~ lat + long + hour + wday + month, data = taxi)

plot(fitted_tree)
text(fitted_tree)

summary(fitted_tree)

install.packages('randomForest')
library(randomForest)

fitted_forest <- randomForest(total ~ lat + long + hour + wday + month, data = taxi, ntree = 80, sampsize = 10000)

fitted_forest

taxi$pred_total <- fitted_forest$predicted

ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') + 
  stat_summary_2d(data = taxi, aes(x = long, y = lat, z = pred_total,), fun = mean, bins = 60, alpha = 0.6) +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Log fare+tip')

mean_if_enough_data <- function(x) { 
  ifelse( length(x) >= 15, mean(x), NA) 
}

ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') + 
  stat_summary_2d(data = taxi, aes(x = long, y = lat, z = total,), fun = mean_if_enough_data, bins = 60, alpha = 0.6) +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Log fare+tip')


spends_most_on_trips <- 'downtown'






