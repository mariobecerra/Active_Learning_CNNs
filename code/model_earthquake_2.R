library(keras)
library(tidyverse)


base_model = application_mobilenet(include_top = FALSE, weights = "imagenet")

# add our custom layers
predictions <- base_model$output %>% 
  layer_global_average_pooling_2d() %>% 
  layer_dense(units = 256, activation = 'relu') %>% 
  layer_dense(units = 2, activation = 'softmax')