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

seed_i = 1729

ix_train = sample(1:dim(x_all)[1], 15000)
ix_val = setdiff(1:dim(x_all)[1], ix_train)

x_train = x_all[ix_train, , , , drop = F]
y_train = y_all_cat[ix_train, ]

x_val = x_all[ix_val, , , , drop = F]
y_val = y_all_cat[ix_val, ]

n_epochs = 250

model = cats_dogs_model()

history = model %>% 
  fit(
    x_train, y_train,
    batch_size = batch_size,
    epochs = n_epochs,
    verbose = 2,
    validation_data = list(x_val, y_val)
  )


save_model_hdf5(model, model_file_name)
saveRDS(history, paste0(dest_folder, "cats_dogs_train_history_", acq_fun, "_", i_str, '.rds'))





