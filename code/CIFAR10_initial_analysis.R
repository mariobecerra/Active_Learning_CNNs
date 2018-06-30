library(keras)
library(reticulate)
library(grid)
library(tidyverse)

# source_python("unpickle_cifar.py")
# 
# meta = unpickle("../data/cifar-10-batches-py/batches.meta")
# 
# data_batch_1 = unpickle("../data/cifar-10-batches-py/data_batch_1")
# class(data_batch_1)
# glimpse(data_batch_1)


cifar10 <- dataset_cifar10()

glimpse(cifar10)

catalog = tibble(label = 0:9,
                 meaning = meta$label_names)

x_all <- cifar10$train$x/255
x_test <- cifar10$test$x/255
y_all <- to_categorical(cifar10$train$y, num_classes = 10)
y_test <- to_categorical(cifar10$test$y, num_classes = 10)

# plot_image <- function(ix, object, catalog){
#   dat = object$data
#   max_dat = max(unlist(dat))
#   if(max_dat != 1) {
#     img = dat[ix,]/max_dat
#   } else {
#     img = dat[ix,]
#   }
#   
#   col <- rgb(matrix(img[1:1024], ncol = 32, byrow = T), 
#              matrix(img[1025:2048], ncol = 32, byrow = T), 
#              matrix(img[2049:3072], ncol = 32, byrow = T))
#   
#   dim(col) <- c(32, 32)
#   
#   label = object$labels[ix]
#   label_string = catalog[label+1,2]
#   label_out = paste0(label_string, ", ", object$filenames[ix])
#   
#   print(label_out)
#   grid.raster(col, interpolate=FALSE)
# }
# 
# plot_image(1, data_batch_1, catalog)
# plot_image(2, data_batch_1, catalog)
# plot_image(3, data_batch_1, catalog)
# plot_image(4, data_batch_1, catalog)
# plot_image(14, data_batch_1, catalog)
# plot_image(149, data_batch_1, catalog)




plot_image <- function(img, label, catalog){
  
  max_dat = max(unlist(img))
  if(max_dat != 1) {
    img = img/max_dat
  } 
  
  col <- rgb(img[,,1], 
             img[,,2],
             img[,,3])
  

  dim(col) <- c(32, 32)
  
  label_string = as.character(catalog[label+1,2])
  
  print(label_string)
  grid.raster(col, interpolate=FALSE)
}

plot_image_ix <- function(ix, cifar10){
  plot_image(cifar10$train$x[ix,,,], cifar10$train$y[ix], catalog)  
}

plot_image_ix(1, cifar10)
plot_image_ix(2, cifar10)
plot_image_ix(3, cifar10)
plot_image_ix(4, cifar10)
