library(reticulate)
library(keras)
library(tidyverse)
library(here)

# Not the best practice, but this is so I don't have to change all files.
setwd(here("code"))

#################################################################################
#################################################################################
## Source R and Python scripts
#################################################################################
#################################################################################

source("MNIST_utils.R")
np <- import("numpy")
source_python("utils.py")
source_python("MNIST_utils.py")

#################################################################################
#################################################################################
## Create variables
#################################################################################
#################################################################################

num_classes = 10
batch_size = 128

# input image dimensions
img_rows = 28 
img_cols = 28

#################################################################################
#################################################################################
## Load dataset and reshape
#################################################################################
#################################################################################

mnist <- dataset_mnist()

x_all <- mnist$train$x
y_all <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# reshape and rescale
if(k_image_data_format() == 'channels_first'){
  x_all <- array_reshape(x_all, c(nrow(x_all), 1, img_rows, img_cols)) / 255
  x_test <- array_reshape(x_test, c(nrow(x_test), 1, img_rows, img_cols)) / 255
} else{
  x_all <- array_reshape(x_all, c(nrow(x_all), img_rows, img_cols, 1)) / 255
  x_test <- array_reshape(x_test, c(nrow(x_test), img_rows, img_cols, 1)) / 255
}

# convert class vectors to binary class matrices
y_all_cat = to_categorical(y_all, num_classes)
y_test_cat = to_categorical(y_test, num_classes)

cat("Converted class vectors\n")


#################################################################################
#################################################################################
## Active Learning
#################################################################################
#################################################################################


seeds = c(1729, 4104, 13832)

for(i in seq_along(seeds)){
  seed_i = seeds[i]
  initial_pool_train_val = create_initial_pool_train_val(y_all, seed_i)
  
  ix_train = initial_pool_train_val$ix_train
  ix_val = initial_pool_train_val$ix_val
  ix_pool = initial_pool_train_val$ix_pool
  
  
  # Run for random acquisition
  random_acquisition(
    n_acq_steps = 100, 
    ix_train = initial_pool_train_val$ix_train, 
    ix_val = initial_pool_train_val$ix_val, 
    ix_pool = initial_pool_train_val$ix_pool, 
    x_all = x_all, 
    y_all = y_all, 
    x_test = x_test, 
    y_test = y_test, 
    n_epochs = 50,
    seed = seed_i)
  
  
  # Run for variation ratios
  acquire_observations(
    acq_fun = 'var_ratios', 
    n_acq_steps = 100, 
    ix_train = initial_pool_train_val$ix_train, 
    ix_val = initial_pool_train_val$ix_val, 
    ix_pool = initial_pool_train_val$ix_pool, 
    x_all = x_all, 
    y_all = y_all, 
    x_test = x_test, 
    y_test = y_test, 
    n_epochs = 50,
    nb_MC_samples = 100)
  
  
  
  # Run for BALD
  acquire_observations(
    acq_fun = 'bald', 
    n_acq_steps = 100, 
    ix_train = initial_pool_train_val$ix_train, 
    ix_val = initial_pool_train_val$ix_val, 
    ix_pool = initial_pool_train_val$ix_pool, 
    x_all = x_all, 
    y_all = y_all, 
    x_test = x_test, 
    y_test = y_test, 
    n_epochs = 50,
    nb_MC_samples = 100)
  
  
  
  # Run for predictive entropy
  acquire_observations(
    acq_fun = 'predictive_entropy', 
    n_acq_steps = 100, 
    ix_train = initial_pool_train_val$ix_train, 
    ix_val = initial_pool_train_val$ix_val, 
    ix_pool = initial_pool_train_val$ix_pool, 
    x_all = x_all, 
    y_all = y_all, 
    x_test = x_test, 
    y_test = y_test, 
    n_epochs = 50,
    nb_MC_samples = 100)
  
  
  
  
  # Run for FREQUENTIST var ratios
  frequentist_acquisition(
    acq_fun = 'freq_var_ratios', 
    n_acq_steps = 100, 
    ix_train = initial_pool_train_val$ix_train, 
    ix_val = initial_pool_train_val$ix_val, 
    ix_pool = initial_pool_train_val$ix_pool, 
    x_all = x_all, 
    y_all = y_all, 
    x_test = x_test, 
    y_test = y_test, 
    n_epochs = 50)
  
  
  
  
  # Run for FREQUENTIST predictive entropy
  frequentist_acquisition(
    acq_fun = 'freq_predictive_entropy', 
    n_acq_steps = 100, 
    ix_train = initial_pool_train_val$ix_train, 
    ix_val = initial_pool_train_val$ix_val, 
    ix_pool = initial_pool_train_val$ix_pool, 
    x_all = x_all, 
    y_all = y_all, 
    x_test = x_test, 
    y_test = y_test, 
    n_epochs = 50)
  
  
}

