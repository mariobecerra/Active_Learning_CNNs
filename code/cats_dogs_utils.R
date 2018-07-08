#################################################################################
#################################################################################
## My functions
#################################################################################
#################################################################################


#################################################################################
## Define model
#################################################################################

cats_dogs_model <- function(){
  img_rows = 32
  img_cols = 32
  if(k_image_data_format() == 'channels_first'){
    input_shape = c(3, img_rows, img_cols)
  } else{
    input_shape = c(img_rows, img_cols, 3)
  }
  
  # Initialize sequential model
  model <- keras_model_sequential()
  model %>%
    # Start with hidden 2D convolutional layer being fed 32x32 pixel images
    layer_conv_2d(
      filter = 32, 
      kernel_size = c(3,3), 
      padding = "same", 
      input_shape = input_shape
    ) %>%
    layer_activation("relu") %>%
    layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
    layer_activation("relu") %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_dropout(0.25) %>%
    layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same") %>%
    layer_activation("relu") %>%
    layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
    layer_activation("relu") %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_dropout(0.25) %>%
    layer_flatten() %>%
    layer_dense(64) %>%
    layer_activation("relu") %>%
    layer_dropout(0.5) %>%
    layer_dense(1) %>%
    layer_activation("sigmoid")
  
  opt <- optimizer_rmsprop(lr = 0.0001, decay = 1e-6)
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = opt,
    metrics = "accuracy"
  )
  
  return(model)  
}



#################################################################################
## Creaste initial sets
#################################################################################

create_initial_train <- function(y_all, seed = 2018){
  set.seed(seed)
  n = 50
  
  y_temp = tibble(y = y_all) %>% 
    mutate(ix = 1:nrow(.)) %>% 
    arrange(y)
  
  indices_df = map_df(0:1, function(i){
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
  ix_val = shuffled_indices[1:500]
  ix_pool = shuffled_indices[501:length(shuffled_indices)]  
  return(list(
    ix_train = ix_train,
    ix_val = ix_val,
    ix_pool = ix_pool
  ))
}


#################################################################################
## Acquire 1/3
#################################################################################

acquire_observations <- function(
  acq_fun, n_acq_steps, 
  ix_train, ix_val, ix_pool, 
  x_all, y_all, 
  x_test, y_test, 
  n_epochs = 50,
  n_images_per_iter = 10,
  nb_MC_samples = 100){
  
  pool_subset = 2000
  
  dir.create("../out/cats_dogs/", showWarnings = F)
  dest_folder = paste0("../out/cats_dogs/", acq_fun, "/")
  dir.create(dest_folder)
  
  accuracies_file_name = paste0(dest_folder, "cats_dogs_accuracies_so_far_", acq_fun, ".csv")
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
    cats_dogs_samples_file_name = paste0(dest_folder, "cats_dogs_samples_", acq_fun, "_", i_str, ".npy")
    cats_dogs_samples_test_file_name = paste0(dest_folder, "cats_dogs_samples_test_", acq_fun, "_", i_str, ".npy")
    model_file_name = paste0(dest_folder, "cats_dogs_model_", acq_fun, "_", i_str, '.h5')
    
    cat("\t\tIter:", i, "\n")
    
    if(is.null(train_pool_ix[[i]])){
      # If ith entry is null, it means that this hasn't been run before so
      # we gotta compute uncertainties to get new examples
      x_train = x_all[ix_train, , , , drop = F]
      y_train = y_all_cat[ix_train, ]
      if(!file.exists(cats_dogs_samples_file_name)){
        # If there's no MC samples file, either load the model, or compute it
        if(file.exists(model_file_name)){
          # If model file exists, loads it
          cat("\t\t\tLoading model from", model_file_name, "\n")
          model = load_model_hdf5(model_file_name)
        } else {
          # If model file doesn't exist, then fit the model
          n_train = dim(x_train)[1]
          #model = cats_dogs_model(n_train)
          model = cats_dogs_model()
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
          saveRDS(history, paste0(dest_folder, "cats_dogs_train_history_", acq_fun, "_", i_str, '.rds'))
          cat("\t\t\tModel saved in", model_file_name, "and history RDS file created\n")
        }
        
        ix_pool_sample = sample(ix_pool, pool_subset)
        
        # Compute MC samples
        cat("\t\t\tComputing pool samples\n")
        MC_samples = get_mc_predictions(model, x_all[ix_pool_sample, , , , drop = F], nb_iter = nb_MC_samples, batch_size = 256)
        cat("\t\t\tSaving pool samples in", cats_dogs_samples_file_name, "\n")
        np$save(cats_dogs_samples_file_name, MC_samples)
        cat("\t\t\tPool samples saved in", cats_dogs_samples_file_name, "\n")
      } else {
        # If there is an MC samples file, then just load it
        cat("\t\t\tLoading pool samples from ", cats_dogs_samples_file_name, "\n")
        MC_samples = np$load(cats_dogs_samples_file_name)
        cat("\t\t\tSamples loaded ", cats_dogs_samples_file_name, "\n")
      }
      
      # Compute uncertainties
      if(acq_fun == 'predictive_entropy')
        acq_func_values = predictive_entropy(MC_samples)
      if(acq_fun == 'var_ratios')
        acq_func_values = variation_ratios(MC_samples)
      if(acq_fun == 'bald')
        acq_func_values = BALD(MC_samples)
      
      # get the 10 points with highest entropy value
      #### HABÍA PROBLEMAS CON LOS ÍNDICES!!!!
      ## id_highest_uncertainty = order(acq_func_values, decreasing = T)[1:10]
      id_highest_uncertainty = ix_pool_sample[order(acq_func_values, decreasing = T)[1:n_images_per_iter]]
      # Save indices for this iteration
      train_pool_ix[[i]] = list(
        ix_pool = ix_pool,
        #ix_pool_sample = ix_pool_sample,
        ix_train = ix_train,
        id_highest_uncertainty = id_highest_uncertainty
      )
      saveRDS(train_pool_ix, train_pool_ix_file_name)
      cat("\t\t\tRDS file train_pool_ix updated\n")
      
      # Update pool and train indices
      ix_train = c(ix_train, id_highest_uncertainty)
      ix_pool = setdiff(ix_pool, id_highest_uncertainty)
      
      # # This should be thought through:
      # # if the id's of highest uncertainty examples have been computed but the test
      # # predictions haven't been made, then accuracies won't be computed.
      # # But this case won't likely happen.
      # if(file.exists(cats_dogs_samples_test_file_name)){
      #   # test set MC samples file exists
      #   cat("\t\t\tLoading test samples from", cats_dogs_samples_test_file_name, "\n")
      #   MC_samples_test = np$load(cats_dogs_samples_test_file_name)
      # } else{
      #   cat("\t\t\tComputing test samples\n")
      #   MC_samples_test = get_mc_predictions(model, x_test, nb_iter=nb_MC_samples, batch_size=256)
      #   cat("\t\t\tSaving test samples in", cats_dogs_samples_test_file_name, "\n")
      #   np$save(cats_dogs_samples_test_file_name, MC_samples_test)
      #   cat("\t\t\tTest samples saved in", cats_dogs_samples_test_file_name, "\n")
      # }
      # 
      # test_preds = predict_MC(MC_samples_test)
      test_preds = predict_classes(model, x_test, batch_size = 256)
      
      accuracy = mean(y_test == test_preds)
      accuracies[i, 2] = accuracy
      cat("\t\t\tAccuracy for this iteration:", accuracy, "\n")
      cat("\t\t\tSaving accuracies so far...")
      write_csv(accuracies, accuracies_file_name)
      cat("Accuracies saved.\n\n")
      
    } else {
      # If the ith element isn't null means that uncertainties have been previously computed
      cat("\t\t\tIndex file exists\n")
      ix_train = c(train_pool_ix[[i]]$ix_train, train_pool_ix[[i]]$id_highest_uncertainty)
      ix_pool = setdiff(train_pool_ix[[i]]$ix_pool, train_pool_ix[[i]]$id_highest_uncertainty)
    }
  } # end for loop
  cat("\n\nLoop ended.\n\n\n")
}





#################################################################################
## Acquire 2/3
#################################################################################

random_acquisition <- function(
  n_acq_steps, 
  ix_train, ix_val, ix_pool, 
  x_all, y_all, 
  x_test, y_test, 
  n_epochs = 50,
  n_images_per_iter = 10,
  seed = 201804){
  
  set.seed(seed)
  acq_fun = 'random'
  
  dir.create("../out/cats_dogs/", showWarnings = F)
  dest_folder = paste0("../out/cats_dogs/random_acq/")
  dir.create(dest_folder)
  
  accuracies_file_name = paste0(dest_folder, "cats_dogs_accuracies_so_far_", acq_fun, ".csv")
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
    cats_dogs_samples_test_file_name = paste0(dest_folder, "cats_dogs_samples_test_", acq_fun, "_", i_str, ".npy")
    model_file_name = paste0(dest_folder, "cats_dogs_model_", acq_fun, "_", i_str, '.h5')
    
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
        # model = cats_dogs_model(n_train)
        model = cats_dogs_model()
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
        saveRDS(history, paste0(dest_folder, "cats_dogs_train_history_", acq_fun, "_", i_str, '.rds'))
        cat("\t\t\tModel saved in", model_file_name, "and history RDS file created\n")
      }
      
      # No need to sample cuz ix_pool is already shuffled
      # If I sample, then results change each time I run this
      #new_train_examples = sample(ix_pool, 10)
      new_train_examples = ix_pool[1:n_images_per_iter]
      
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
      
      # # This should be thought through:
      # # if the id's of highest uncertainty examples have been computed but the test
      # # predictions haven't been made, then accuracies won't be computed.
      # # But this case won't likely happen.
      # if(file.exists(cats_dogs_samples_test_file_name)){
      #   # test set MC samples file exists
      #   cat("\t\t\tLoading test samples from", cats_dogs_samples_test_file_name, "\n")
      #   MC_samples_test = np$load(cats_dogs_samples_test_file_name)
      # } else{
      #   cat("\t\t\tComputing test samples\n")
      #   MC_samples_test = get_mc_predictions(model, x_test, nb_iter=nb_MC_samples, batch_size=256)
      #   cat("\t\t\tSaving test samples in", cats_dogs_samples_test_file_name, "\n")
      #   np$save(cats_dogs_samples_test_file_name, MC_samples_test)
      #   cat("\t\t\tTest samples saved in", cats_dogs_samples_test_file_name, "\n")
      # }
      # 
      # test_preds = predict_MC(MC_samples_test)
      test_preds = predict_classes(model, x_test, batch_size = 256)
      
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
## Acquire 3/3
#################################################################################

frequentist_acquisition <- function(
  acq_fun, n_acq_steps, 
  ix_train, ix_val, ix_pool, 
  x_all, y_all, 
  x_test, y_test, 
  n_epochs = 50,
  n_images_per_iter = 10){
  
  pool_subset = 2000
  
  dir.create("../out/cats_dogs/", showWarnings = F)
  dest_folder = paste0("../out/cats_dogs/", acq_fun, "/")
  dir.create(dest_folder)
  
  accuracies_file_name = paste0(dest_folder, "cats_dogs_accuracies_so_far_", acq_fun, ".csv")
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
    # cats_dogs_samples_file_name = paste0(dest_folder, "cats_dogs_samples_", acq_fun, "_", i_str, ".npy")
    # cats_dogs_samples_test_file_name = paste0(dest_folder, "cats_dogs_samples_test_", acq_fun, "_", i_str, ".npy")
    model_file_name = paste0(dest_folder, "cats_dogs_model_", acq_fun, "_", i_str, '.h5')
    
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
        # model = cats_dogs_model(n_train)
        model = cats_dogs_model()
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
        saveRDS(history, paste0(dest_folder, "cats_dogs_train_history_", acq_fun, "_", i_str, '.rds'))
        cat("\t\t\tModel saved in", model_file_name, "and history RDS file created\n")
      }
      
      ix_pool_sample = sample(ix_pool, pool_subset)
      
      # Compute pool predictions
      cat("\t\t\tComputing pool predictions\n")
      frequentist_predictions = predict(model, x_all[ix_pool_sample, , , , drop = F], batch_size = 256)
      # MC_samples = get_mc_predictions(model, x_all[ix_pool, , , , drop = F], nb_iter = nb_MC_samples, batch_size = 256)
      
      # Compute uncertainties
      if(acq_fun == 'freq_predictive_entropy'){
        # - np.sum(np.log(p_y_c + 1e-10)*p_y_c, axis = 1)
        acq_func_values = -apply(log(frequentist_predictions + 1e-10)*frequentist_predictions, 1, sum)
      }
      if(acq_fun == 'freq_var_ratios'){
        acq_func_values = 1 - apply(frequentist_predictions, 1, max)
      } 
      # if(acq_fun == 'freq_bald')
      #   acq_func_values = BALD(MC_samples)
      
      # get the 10 points with highest entropy value
      #### HABÍA PROBLEMAS CON LOS ÍNDICES!!!!
      ## id_highest_uncertainty = order(acq_func_values, decreasing = T)[1:n_images_per_iter]
      id_highest_uncertainty = ix_pool_sample[order(acq_func_values, decreasing = T)[1:n_images_per_iter]]
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
      
      # # This should be thought through:
      # # if the id's of highest uncertainty examples have been computed but the test
      # # predictions haven't been made, then accuracies won't be computed.
      # # But this case won't likely happen.
      # if(file.exists(cats_dogs_samples_test_file_name)){
      #   # test set MC samples file exists
      #   cat("\t\t\tLoading test samples from", cats_dogs_samples_test_file_name, "\n")
      #   MC_samples_test = np$load(cats_dogs_samples_test_file_name)
      # } else{
      #   cat("\t\t\tComputing test samples\n")
      #   MC_samples_test = get_mc_predictions(model, x_test, nb_iter=nb_MC_samples, batch_size=256)
      #   cat("\t\t\tSaving test samples in", cats_dogs_samples_test_file_name, "\n")
      #   np$save(cats_dogs_samples_test_file_name, MC_samples_test)
      #   cat("\t\t\tTest samples saved in", cats_dogs_samples_test_file_name, "\n")
      # }
      
      test_preds = predict_classes(model, x_test, batch_size = 256)
      
      accuracy = mean(y_test == test_preds)
      accuracies[i, 2] = accuracy
      cat("\t\t\tAccuracy for this iteration:", accuracy, "\n")
      cat("\t\t\tSaving accuracies so far...")
      write_csv(accuracies, accuracies_file_name)
      cat("Accuracies saved.\n\n")
      
    } else {
      # If the ith element isn't null means that uncertainties have been previously computed
      cat("\t\t\tIndex file exists\n")
      ix_train = c(train_pool_ix[[i]]$ix_train, train_pool_ix[[i]]$id_highest_uncertainty)
      ix_pool = setdiff(train_pool_ix[[i]]$ix_pool, train_pool_ix[[i]]$id_highest_uncertainty)
    }
  } # end for loop
  cat("\n\nLoop ended.\n\n\n")
}





