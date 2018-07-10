library(reticulate)
library(keras)
library(tidyverse)

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

if(file.exists("../out/cats_dogs/dat_train_64x64.rds")){
  cats_dogs <- readRDS("../out/cats_dogs/dat_train_64x64.rds")
} else{
  stop("Data file not found.\n\n")
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

# Random initial set of 20 points for training, 100 for validation and the rest as pooling set
initial_pool_train_val = create_initial_pool_train_val(y_all, 2018)

ix_train = initial_pool_train_val$ix_train
ix_val = initial_pool_train_val$ix_val
ix_pool = initial_pool_train_val$ix_pool




# Run funciton for random acquisition
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
  seed = 201804)





temp = paste0("Start FREQUENTIST var ratios: ", as.character(Sys.time()))
cat(temp, file = "../out/cats_dogs/finish_time.txt", append = T, sep = "\n")
# Run funciton for FREQUENTIST var ratios
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
temp = paste0("Finish FREQUENTIST var ratios: ", as.character(Sys.time()))
cat(temp, file = "../out/cats_dogs/finish_time.txt", append = T, sep = "\n")




temp = paste0("Start FREQUENTIST predictive entropy: ", as.character(Sys.time()))
cat(temp, file = "../out/cats_dogs/finish_time.txt", append = T, sep = "\n")
# Run funciton for FREQUENTIST predictive entropy
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
temp = paste0("Finish FREQUENTIST predictive entropy: ", as.character(Sys.time()))
cat(temp, file = "../out/cats_dogs/finish_time.txt", append = T, sep = "\n")



temp = paste0("Start var ratios: ", as.character(Sys.time()))
cat(temp, file = "../out/cats_dogs/finish_time.txt", append = T, sep = "\n")
# Run funciton for variation ratios
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
temp = paste0("Finish var ratios: ", as.character(Sys.time()))
cat(temp, file = "../out/cats_dogs/finish_time.txt", append = T, sep = "\n")



temp = paste0("Start BALD: ", as.character(Sys.time()))
cat(temp, file = "../out/cats_dogs/finish_time.txt", append = T, sep = "\n")
# Run funciton for BALD
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
temp = paste0("Finish BALD: ", as.character(Sys.time()))
cat(temp, file = "../out/cats_dogs/finish_time.txt", append = T, sep = "\n")




temp = paste0("Start pred ent: ", as.character(Sys.time()))
cat(temp, file = "../out/cats_dogs/finish_time.txt", append = T, sep = "\n")
# Run funciton for predictive entropy
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
temp = paste0("Finish pred ent: ", as.character(Sys.time()))
cat(temp, file = "../out/cats_dogs/finish_time.txt", append = T, sep = "\n")




# cat(as.character(Sys.time()), file = "../out/cats_dogs/finish_time.txt", append = T, sep = "\n")