library(reticulate)
library(keras)
library(tidyverse)

#################################################################################
#################################################################################
## Source R and Python scripts
#################################################################################
#################################################################################

source("utils_CIFAR10.R")
np <- import("numpy")
source_python("utils.py")


#################################################################################
#################################################################################
## Create variables
#################################################################################
#################################################################################

num_classes = 10
batch_size = 128

# input image dimensions
img_rows = 32
img_cols = 32

#################################################################################
#################################################################################
## Load dataset and reshape
#################################################################################
#################################################################################

cifar10 <- dataset_cifar10()

x_all <- cifar10$train$x
y_all <- as.integer(cifar10$train$y)
x_test <- cifar10$test$x
y_test <- as.integer(cifar10$test$y)

# reshape and rescale
if(k_image_data_format() == 'channels_first'){
  x_all <- array_reshape(x_all, c(nrow(x_all), 3, img_rows, img_cols)) / 255
  x_test <- array_reshape(x_test, c(nrow(x_test), 3, img_rows, img_cols)) / 255
} else{
  # x_all <- array_reshape(x_all, c(nrow(x_all), img_rows, img_cols, 3)) / 255
  # x_test <- array_reshape(x_test, c(nrow(x_test), img_rows, img_cols, 3)) / 255
  x_all <- x_all / 255
  x_test <- x_test / 255
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
  n_acq_steps = 100, 
  ix_train = initial_pool_train_val$ix_train, 
  ix_val = initial_pool_train_val$ix_val, 
  ix_pool = initial_pool_train_val$ix_pool, 
  x_all = x_all, 
  y_all = y_all, 
  x_test = x_test, 
  y_test = y_test, 
  n_epochs = 50,
  seed = 201804)


temp = paste0("Start var ratios: ", as.character(Sys.time()))
cat(temp, file = "../out/CIFAR10/finish_time.txt", append = T, sep = "\n")
# Run funciton for variation ratios
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
  #nb_MC_samples = 100
  nb_MC_samples = 100
)
temp = paste0("Finish var ratios: ", as.character(Sys.time()))
cat(temp, file = "../out/CIFAR10/finish_time.txt", append = T, sep = "\n")



temp = paste0("Start BALD: ", as.character(Sys.time()))
cat(temp, file = "../out/CIFAR10/finish_time.txt", append = T, sep = "\n")
# Run funciton for BALD
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
temp = paste0("Finish BALD: ", as.character(Sys.time()))
cat(temp, file = "../out/CIFAR10/finish_time.txt", append = T, sep = "\n")




temp = paste0("Start pred ent: ", as.character(Sys.time()))
cat(temp, file = "../out/CIFAR10/finish_time.txt", append = T, sep = "\n")
# Run funciton for predictive entropy
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
temp = paste0("Finish pred ent: ", as.character(Sys.time()))
cat(temp, file = "../out/CIFAR10/finish_time.txt", append = T, sep = "\n")




temp = paste0("Start FREQUENTIST var ratios: ", as.character(Sys.time()))
cat(temp, file = "../out/CIFAR10/finish_time.txt", append = T, sep = "\n")
# Run funciton for FREQUENTIST var ratios
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
temp = paste0("Finish FREQUENTIST var ratios: ", as.character(Sys.time()))
cat(temp, file = "../out/CIFAR10/finish_time.txt", append = T, sep = "\n")




temp = paste0("Start FREQUENTIST predictive entropy: ", as.character(Sys.time()))
cat(temp, file = "../out/CIFAR10/finish_time.txt", append = T, sep = "\n")
# Run funciton for FREQUENTIST predictive entropy
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
temp = paste0("Finish FREQUENTIST predictive entropy: ", as.character(Sys.time()))
cat(temp, file = "../out/CIFAR10/finish_time.txt", append = T, sep = "\n")

# cat(as.character(Sys.time()), file = "../out/CIFAR10/finish_time.txt", append = T, sep = "\n")
