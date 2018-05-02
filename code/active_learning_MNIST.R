library(reticulate)
library(keras)
library(tidyverse)

#################################################################################
#################################################################################
## My functions
#################################################################################
#################################################################################

create_initial_train <- function(y_all, seed = 2018){
  set.seed(seed)
  n = 2
  
  y_temp = tibble(y = y_all) %>% 
    mutate(ix = 1:nrow(.)) %>% 
    arrange(y)
  
  indices_df = map_df(0:9, function(i){
    filt = y_temp %>% 
      filter(y == i)
    out = tibble(y = rep(as.integer(i), n),
                 ix = sample(filt$ix, n))
    return(out)
  })  
  return(indices_df$ix)
}


create_initial_pool_train_val <- function(y_all, seed = 2018){
  set.seed(seed)
  ix_train = create_initial_train(y_all, seed)
  available_idx = setdiff(1:length(y_all), ix_train)
  shuffled_indices = sample(available_idx, length(available_idx))
  ix_val = shuffled_indices[1:100]
  ix_pool = shuffled_indices[101:length(shuffled_indices)]  
  return(list(
    ix_train = ix_train,
    ix_val = ix_val,
    ix_pool = ix_pool
  ))
}

acquire_observations <- function(
  acq_fun, n_acq_steps, 
  ix_train, ix_val, ix_pool, 
  x_all, y_all, 
  x_test, y_test, 
  n_epochs = 50,
  nb_MC_samples = 100){
  
  dir.create("../out/MNIST/", showWarnings = F)
  dest_folder = paste0("../out/MNIST/", acq_fun, "/")
  dir.create(dest_folder)
  
  accuracies_file_name = paste0(dest_folder, "MNIST_accuracies_so_far_", acq_fun, ".csv")
  train_pool_ix_file_name = paste0(dest_folder, "train_pool_ix_", acq_fun, ".rds")
  
  x_val = x_all[ix_val, , , , drop = F]
  y_val = y_all_cat[ix_val, ]
  
  if(file.exists(accuracies_file_name)){
    cat("\tReading accuracies csv file (", accuracies_file_name, ")\n")
    accuracies <- read_csv(accuracies_file_name, col_types = cols(col_character(), col_double(), col_integer())) 
  } else{
    cat("\tCreating accuracies dataframe\n")
    accuracies = tibble(acq_fun = acq_fun,
                        accuracy = rep(NA, n_acq_steps)) %>% 
      mutate(iter = 1:nrow(.)) 
    write_csv(accuracies, accuracies_file_name)
    cat("\tAccuracies csv file created and saved in", accuracies_file_name, "\n")
  }
  
  if(file.exists(train_pool_ix_file_name)) {
    # If file with the indices of each iteration exists, load it
    cat("\tReading train_pool_ix rds file (", train_pool_ix_file_name, ")\n")
    train_pool_ix = readRDS(train_pool_ix_file_name)
  } else {
    # Otherwise, create empty list
    cat("\tCreating train_pool_ix list\n")
    train_pool_ix = vector("list", length = n_acq_steps)
    names(train_pool_ix) = paste0("iter_", stringr::str_pad(1:n_acq_steps, 3, pad = "0")) 
    saveRDS(train_pool_ix, train_pool_ix_file_name)
    cat("\tRDS file train_pool_ix saved in", train_pool_ix_file_name, "\n")
  }
  
  # Begin loop
  for(i in 1:n_acq_steps){
    i_str = stringr::str_pad(i, 3, pad = "0")
    MNIST_samples_file_name = paste0(dest_folder, "MNIST_samples_", acq_fun, "_", i_str, ".npy")
    MNIST_samples_test_file_name = paste0(dest_folder, "MNIST_samples_test_", acq_fun, "_", i_str, ".npy")
    model_file_name = paste0(dest_folder, "MNIST_model_", acq_fun, "_", i_str, '.h5')
    
    cat("\t\tIter:", i, "\n")
    
    if(is.null(train_pool_ix[[i]])){
      # If ith entry is null, it means that this hasn't been run before so
      # we gotta compute uncertainties to get new examples
      x_train = x_all[ix_train, , , , drop = F]
      y_train = y_all_cat[ix_train, ]
      if(!file.exists(MNIST_samples_file_name)){
        # If there's no MC samples file, either load the model, or compute it
        if(file.exists(model_file_name)){
          # If model file exists, loads it
          cat("\t\t\tLoading model from", model_file_name, "\n")
          model = load_model_hdf5(model_file_name)
        } else {
          # If model file doesn't exist, then fit the model
          n_train = dim(x_train)[1]
          model = MNIST_model(n_train)
          cat("\t\t\tTraining model with", n_train, "samples...")
          history = model %>% 
            fit(
              x_train, y_train,
              batch_size = batch_size,
              epochs = n_epochs,
              verbose = 0,
              validation_data = list(x_val, y_val)
            )
          cat("Training finished\n")
          cat("\t\t\t\tTraining accuracy:", history$metrics$acc[n_epochs], "\n")
          cat("\t\t\t\tValidation accuracy:", history$metrics$val_acc[n_epochs], "\n")
          cat("\t\t\tSaving model in", model_file_name, "\n")
          save_model_hdf5(model, model_file_name)
          saveRDS(history, paste0(dest_folder, "MNIST_train_history_", acq_fun, "_", i_str, '.rds'))
          cat("\t\t\tModel saved in", model_file_name, "and history RDS file created\n")
        }
        # Compute MC samples
        cat("\t\t\tComputing pool samples\n")
        MC_samples = get_mc_predictions(model, x_all[ix_pool, , , , drop = F], nb_iter = nb_MC_samples, batch_size = 256)
        cat("\t\t\tSaving pool samples in", MNIST_samples_file_name, "\n")
        np$save(MNIST_samples_file_name, MC_samples)
        cat("\t\t\tPool samples saved in", MNIST_samples_file_name, "\n")
      } else {
        # If there is an MC samples file, then just load it
        cat("\t\t\tLoading pool samples from ", MNIST_samples_file_name, "\n")
        MC_samples = np$load(MNIST_samples_file_name)
        cat("\t\t\tSamples loaded ", MNIST_samples_file_name, "\n")
      }
      
      # Compute uncertainties
      if(acq_fun == 'predictive_entropy')
        acq_func_values = predictive_entropy(MC_samples)
      if(acq_fun == 'var_ratios')
        acq_func_values = variation_ratios(MC_samples)
      if(acq_fun == 'bald')
        acq_func_values = BALD(MC_samples)
      
      # get the 10 points with highest entropy value
      id_highest_uncertainty = order(acq_func_values, decreasing = T)[1:10] 
      # Save indices for this iteration
      train_pool_ix[[i]] = list(
        ix_pool = ix_pool,
        ix_train = ix_train,
        id_highest_uncertainty = id_highest_uncertainty
      )
      saveRDS(train_pool_ix, train_pool_ix_file_name)
      cat("\t\t\tRDS file train_pool_ix updated\n")
      
      # Update pool and train indices
      ix_train = c(ix_train, id_highest_uncertainty)
      ix_pool = setdiff(ix_pool, id_highest_uncertainty)
      
      # This should be thought through:
      # if the id's of highest uncertainty examples have been computed but the test
      # predictions haven't been made, then accuracies won't be computed.
      # But this case won't likely happen.
      if(file.exists(MNIST_samples_test_file_name)){
        # test set MC samples file exists
        cat("\t\t\tLoading test samples from", MNIST_samples_test_file_name, "\n")
        MC_samples_test = np$load(MNIST_samples_test_file_name)
      } else{
        cat("\t\t\tComputing test samples\n")
        MC_samples_test = get_mc_predictions(model, x_test, nb_iter=nb_MC_samples, batch_size=256)
        cat("\t\t\tSaving test samples in", MNIST_samples_test_file_name, "\n")
        np$save(MNIST_samples_test_file_name, MC_samples_test)
        cat("\t\t\tTest samples saved in", MNIST_samples_test_file_name, "\n")
      }
      
      test_preds = predict_MC(MC_samples_test)
      
      accuracy = mean(y_test == test_preds)
      accuracies[i, 2] = accuracy
      cat("\t\t\tAccuracy for this iteration:", accuracy, "\n\n\n")
      cat("\t\t\tSaving accuracies so far...")
      write_csv(accuracies, accuracies_file_name)
      cat("\t\t\tAccuracies saved.\n\n")
      
    } else {
      # If the ith element isn't null means that uncertainties have been previously computed
      cat("\t\t\tIndex file exists\n")
      ix_train = c(train_pool_ix[[i]]$ix_train, train_pool_ix[[i]]$id_highest_uncertainty)
      ix_pool = setdiff(train_pool_ix[[i]]$ix_pool, train_pool_ix[[i]]$id_highest_uncertainty)
    }
  } # end for loop
  cat("\n\nLoop ended.\n\n\n")
}




random_acquisition <- function(
  n_acq_steps, 
  ix_train, ix_val, ix_pool, 
  x_all, y_all, 
  x_test, y_test, 
  nb_MC_samples = 100,
  n_epochs = 50,
  seed = 201804){
  
  set.seed(seed)
  acq_fun = 'random'
  
  dir.create("../out/MNIST/", showWarnings = F)
  dest_folder = paste0("../out/MNIST/random_acq/")
  dir.create(dest_folder)
  
  accuracies_file_name = paste0(dest_folder, "MNIST_accuracies_so_far_", acq_fun, ".csv")
  train_pool_ix_file_name = paste0(dest_folder, "train_pool_ix_", acq_fun, ".rds")
  
  x_val = x_all[ix_val, , , , drop = F]
  y_val = y_all_cat[ix_val, ]
  
  if(file.exists(accuracies_file_name)){
    cat("\tReading accuracies csv file (", accuracies_file_name, ")\n")
    accuracies <- read_csv(accuracies_file_name, col_types = cols(col_character(), col_double(), col_integer())) 
  } else{
    cat("\tCreating accuracies dataframe\n")
    accuracies = tibble(acq_fun = acq_fun,
                        accuracy = rep(NA, n_acq_steps)) %>% 
      mutate(iter = 1:nrow(.))
    write_csv(accuracies, accuracies_file_name)
    cat("\tAccuracies csv file created and saved in", accuracies_file_name, "\n")
  }
  
  if(file.exists(train_pool_ix_file_name)) {
    # If file with the indices of each iteration exists, load it
    cat("\tReading train_pool_ix rds file (", train_pool_ix_file_name, ")\n")
    train_pool_ix = readRDS(train_pool_ix_file_name)
  } else {
    # Otherwise, create empty list
    cat("\tCreating train_pool_ix list\n")
    train_pool_ix = vector("list", length = n_acq_steps)
    names(train_pool_ix) = paste0("iter_", stringr::str_pad(1:n_acq_steps, 3, pad = "0")) 
    saveRDS(train_pool_ix, train_pool_ix_file_name)
    cat("\tRDS file train_pool_ix saved in", train_pool_ix_file_name, "\n")
  }
  
  # Begin loop
  for(i in 1:n_acq_steps){
    i_str = stringr::str_pad(i, 3, pad = "0")
    MNIST_samples_test_file_name = paste0(dest_folder, "MNIST_samples_test_", acq_fun, "_", i_str, ".npy")
    model_file_name = paste0(dest_folder, "MNIST_model_", acq_fun, "_", i_str, '.h5')
    
    cat("\t\tIter:", i, "\n")
    
    if(is.null(train_pool_ix[[i]])){
      # If ith entry is null, it means that this hasn't been run before so
      # we gotta compute uncertainties to get new examples
      x_train = x_all[ix_train, , , , drop = F]
      y_train = y_all_cat[ix_train, ]
      
      if(file.exists(model_file_name)){
        # If model file exists, loads it
        cat("\t\t\tLoading model from", model_file_name, "\n")
        model = load_model_hdf5(model_file_name)
      } else {
        # If model file doesn't exist, then fit the model
        n_train = dim(x_train)[1]
        model = MNIST_model(n_train)
        cat("\t\t\tTraining model with", n_train, "samples...")
        history = model %>% 
          fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = n_epochs,
            verbose = 0,
            validation_data = list(x_val, y_val)
          )
        cat("Training finished\n")
        cat("\t\t\t\tTraining accuracy:", history$metrics$acc[n_epochs], "\n")
        cat("\t\t\t\tValidation accuracy:", history$metrics$val_acc[n_epochs], "\n")
        cat("\t\t\tSaving model in", model_file_name, "\n")
        save_model_hdf5(model, model_file_name)
        saveRDS(history, paste0(dest_folder, "MNIST_train_history_", acq_fun, "_", i_str, '.rds'))
        cat("\t\t\tModel saved in", model_file_name, "and history RDS file created\n")
      }
      
      new_train_examples = sample(ix_pool, 10)
      
      # Save indices for this iteration
      train_pool_ix[[i]] = list(
        ix_pool = ix_pool,
        ix_train = ix_train,
        new_train_examples = new_train_examples
      )
      saveRDS(train_pool_ix, train_pool_ix_file_name)
      
      # Update pool and train indices
      ix_train = c(ix_train, new_train_examples)
      ix_pool = setdiff(ix_pool, new_train_examples)
      
      # This should be thought through:
      # if the id's of highest uncertainty examples have been computed but the test
      # predictions haven't been made, then accuracies won't be computed.
      # But this case won't likely happen.
      if(file.exists(MNIST_samples_test_file_name)){
        # test set MC samples file exists
        cat("\t\t\tLoading test samples from", MNIST_samples_test_file_name, "\n")
        MC_samples_test = np$load(MNIST_samples_test_file_name)
      } else{
        cat("\t\t\tComputing test samples\n")
        MC_samples_test = get_mc_predictions(model, x_test, nb_iter=nb_MC_samples, batch_size=256)
        cat("\t\t\tSaving test samples in", MNIST_samples_test_file_name, "\n")
        np$save(MNIST_samples_test_file_name, MC_samples_test)
        cat("\t\t\tTest samples saved in", MNIST_samples_test_file_name, "\n")
      }
      
      test_preds = predict_MC(MC_samples_test)
      
      accuracy = mean(y_test == test_preds)
      accuracies[i, 2] = accuracy
      cat("\t\t\tAccuracy for this iteration:", accuracy, "\n\n\n")
      cat("\t\t\tSaving accuracies so far...")
      write_csv(accuracies, accuracies_file_name)
      cat("\t\t\tAccuracies saved.\n\n")
    } else {
      # If the ith element isn't null means that uncertainties have been previously computed
      cat("\t\t\tIndex file exists\n")
      ix_train = c(train_pool_ix[[i]]$ix_train, train_pool_ix[[i]]$new_train_examples)
      ix_pool = setdiff(train_pool_ix[[i]]$ix_pool, train_pool_ix[[i]]$new_train_examples)
    }
  } # end for loop
  cat("\n\nLoop ended.\n\n\n")  
}


#################################################################################
#################################################################################
## Source python scripts
#################################################################################
#################################################################################

np <- import("numpy")
source_python("utils.py")
source_python("utils_MNIST.py")

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

#acq_fun_string = ['predictive_entropy', 'var_ratios', 'bald']

# Random initial set of 20 points for training, 100 for validation and the rest as pooling set
# shuffled_indices = as.integer(create_shuffled_indices_MNIST()) + 1
# ix_train = shuffled_indices[1:20]
# ix_val = shuffled_indices[21:120]
# ix_pool = shuffled_indices[121:length(shuffled_indices)]
initial_pool_train_val = create_initial_pool_train_val(y_all, 2018)

ix_train = initial_pool_train_val$ix_train
ix_val = initial_pool_train_val$ix_val
ix_pool = initial_pool_train_val$ix_pool





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
  nb_MC_samples = 20)
  

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
  nb_MC_samples = 20
  )

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
  nb_MC_samples = 20)


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
  nb_MC_samples = 20,
  n_epochs = 50,
  seed = 201804)
