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

source("cats_dogs_utils.R")
np <- import("numpy")
source_python("utils.py")


#################################################################################
#################################################################################
## Create variables
#################################################################################
#################################################################################

num_classes = 2
batch_size = 128

# input image dimensions
img_rows = 64
img_cols = 64

#################################################################################
#################################################################################
## Load dataset and reshape
#################################################################################
#################################################################################

train_folder = "../out/cats_dogs/"
if(!dir.exists(train_folder)) dir.create(train_folder)

train_rds_filename = paste0(train_folder, "dat_train_64x64.rds")
train_data_url = "https://www.dropbox.com/s/9t7p4ng5nu8ogc2/dat_train_64x64.rds?dl=1"

if(file.exists(train_rds_filename)){
  cats_dogs <- readRDS(train_rds_filename)
} else{
  message("Data file not found. Going to download.\n\n")
  download_res = try(download.file(train_data_url, train_rds_filename))
  if(class(download_res) != "try-error") {
    message("Download successful.")
    cats_dogs <- readRDS(train_rds_filename)
  } else{
    stop("Download failed!!!")
  }
}

n_pics = nrow(cats_dogs$x)

set.seed(2018)
ix_train = sample(1:n_pics, size = 20000)
ix_test = setdiff(1:n_pics, ix_train)

x_all <- cats_dogs$x[ix_train,,,]
y_all <- ifelse(cats_dogs$y[ix_train] == "dog", 1, 0)
x_test <- cats_dogs$x[ix_test,,,]
y_test <- ifelse(cats_dogs$y[ix_test] == "dog", 1, 0)

rm(cats_dogs)
gc()

# reshape and rescale
if(k_image_data_format() == 'channels_first'){
  x_all <- array_reshape(x_all, c(nrow(x_all), 3, img_rows, img_cols))
  x_test <- array_reshape(x_test, c(nrow(x_test), 3, img_rows, img_cols))
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
  
  # Random initial set of 20 points for training, 100 for validation and the rest as pooling set
  initial_pool_train_val = create_initial_pool_train_val(y_all, seed_i)
  
  ix_train = initial_pool_train_val$ix_train
  ix_val = initial_pool_train_val$ix_val
  ix_pool = initial_pool_train_val$ix_pool
  
  
  
  
  # Run for random acquisition
  random_acquisition(
    n_acq_steps = 50,
    ix_train = initial_pool_train_val$ix_train,
    ix_val = initial_pool_train_val$ix_val,
    ix_pool = initial_pool_train_val$ix_pool,
    x_all = x_all,
    y_all = y_all,
    x_test = x_test,
    y_test = y_test,
    n_epochs = 200,
    n_images_per_iter = 50,
    seed = seed_i)
  
  
  
  
  
  # Run for FREQUENTIST var ratios
  frequentist_acquisition(
    acq_fun = 'freq_var_ratios',
    n_acq_steps = 50,
    ix_train = initial_pool_train_val$ix_train,
    ix_val = initial_pool_train_val$ix_val,
    ix_pool = initial_pool_train_val$ix_pool,
    x_all = x_all,
    y_all = y_all,
    x_test = x_test,
    y_test = y_test,
    n_epochs = 200,
    n_images_per_iter = 50)
  
  
  
  
  # Run for FREQUENTIST predictive entropy
  frequentist_acquisition(
    acq_fun = 'freq_predictive_entropy',
    n_acq_steps = 50,
    ix_train = initial_pool_train_val$ix_train,
    ix_val = initial_pool_train_val$ix_val,
    ix_pool = initial_pool_train_val$ix_pool,
    x_all = x_all,
    y_all = y_all,
    x_test = x_test,
    y_test = y_test,
    n_epochs = 200,
    n_images_per_iter = 50)
  
  
  
  # Run for variation ratios
  acquire_observations(
    acq_fun = 'var_ratios',
    n_acq_steps = 50,
    ix_train = initial_pool_train_val$ix_train,
    ix_val = initial_pool_train_val$ix_val,
    ix_pool = initial_pool_train_val$ix_pool,
    x_all = x_all,
    y_all = y_all,
    x_test = x_test,
    y_test = y_test,
    n_epochs = 200,
    n_images_per_iter = 50,
    nb_MC_samples = 100
  )
  
  
  # Run for BALD
  acquire_observations(
    acq_fun = 'bald',
    n_acq_steps = 50,
    ix_train = initial_pool_train_val$ix_train,
    ix_val = initial_pool_train_val$ix_val,
    ix_pool = initial_pool_train_val$ix_pool,
    x_all = x_all,
    y_all = y_all,
    x_test = x_test,
    y_test = y_test,
    n_epochs = 200,
    n_images_per_iter = 50,
    nb_MC_samples = 100)
  
  
  
  
  # Run for predictive entropy
  acquire_observations(
    acq_fun = 'predictive_entropy',
    n_acq_steps = 50,
    ix_train = initial_pool_train_val$ix_train,
    ix_val = initial_pool_train_val$ix_val,
    ix_pool = initial_pool_train_val$ix_pool,
    x_all = x_all,
    y_all = y_all,
    x_test = x_test,
    y_test = y_test,
    n_epochs = 200,
    n_images_per_iter = 50,
    nb_MC_samples = 100)
  
  
}


