# Dropout after convolution test
library(tidyverse)
library(reticulate)
library(keras)



create_initial_train <- function(y_all, n_per_class = 2, seed = 2018){
  set.seed(seed)
  
  y_temp = tibble(y = y_all) %>% 
    mutate(ix = 1:nrow(.)) %>% 
    arrange(y)
  
  indices_df = map_df(0:9, function(i){
    filt = y_temp %>% 
      filter(y == i)
    out = tibble(y = rep(as.integer(i), n_per_class),
                 ix = sample(filt$ix, n_per_class))
    return(out)
  })  
  return(indices_df$ix)
}




np <- import("numpy")
source_python("utils.py")
source_python("MNIST_model_dropout.py")

n_epochs = 500

num_classes = 10
batch_size = 128

# input image dimensions
img_rows = 28 
img_cols = 28


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


ix_train_1 = create_initial_train(y_all, 50, seed = 2018)
ix_val = sample(setdiff(1:length(y_all), ix_train_1), 200)

shuffled_pool = sample(setdiff(1:length(y_all), c(ix_val, ix_train_1)), 50000)

x_train = x_all[ix_train_1, , , , drop = F]
y_train = y_all_cat[ix_train_1, ]
x_val = x_all[ix_val, , , , drop = F]
y_val = y_all_cat[ix_val, ]

n_train = dim(x_train)[1]


model_no_droput = MNIST_model(n_train)
cat("\t\t\tTraining model with", n_train, "samples...")
history = model_no_droput %>% 
  fit(
    x_train, y_train,
    batch_size = batch_size,
    epochs = n_epochs,
    verbose = 1,
    validation_data = list(x_val, y_val)
  )


model_with_droput = MNIST_model_extra_dropout(n_train)
history = model_with_droput %>% 
  fit(
    x_train, y_train,
    batch_size = batch_size,
    epochs = n_epochs,
    verbose = 1,
    validation_data = list(x_val, y_val)
  )

# MC predictions of model with extra dropout layer
MC_samples_test_with_dropout = get_mc_predictions(model_with_droput, x_test, nb_iter = 50, batch_size = 500)
MC_class_preds_with_dropout = predict_MC(MC_samples_test_with_dropout)
(accuracy_MC_with_dropout = mean(y_test == MC_class_preds_with_dropout))

# MC predictions of model without extra dropout layer
MC_samples_test_no_dropout = get_mc_predictions(model_no_droput, x_test, nb_iter = 50, batch_size = 500)
MC_class_preds_no_dropout = predict_MC(MC_samples_test_no_dropout)
(accuracy_MC_no_dropout = mean(y_test == MC_class_preds_no_dropout))

# deterministic predictions of model with extra dropout layer
freq_classes_test_with_dropout = predict_classes(model_with_droput, x_test, batch_size = 500)
(accuracy_freq_with_dropout = mean(y_test == freq_classes_test_with_dropout))

# deterministic predictions of model without extra dropout layer
freq_classes_test_no_dropout = predict_classes(model_no_droput, x_test, batch_size = 500)
(accuracy_freq_no_dropout = mean(y_test == freq_classes_test_no_dropout))


accs = tibble(i =1:100, 
              accuracy_MC_with_dropout = rep(NA, 100),
              accuracy_MC_no_dropout = rep(NA, 100),
              accuracy_freq_with_dropout = rep(NA, 100),
              accuracy_freq_no_dropout = rep(NA, 100))

for(i in 1:20){
  ix_temp = ((i-1)*1000 + 1):(i*1000)
  ix_test_sub = shuffled_pool[ix_temp]
  
  # deterministic predictions of model with extra dropout layer
  freq_classes_with_dropout = predict_classes(model_with_droput, x_all[ix_test_sub, , , , drop = F], 
                                              batch_size = 500)
  (accuracy_freq_with_dropout = mean(y_all[ix_test_sub] == freq_classes_with_dropout))
  accs$accuracy_freq_with_dropout[i] = accuracy_freq_with_dropout
  
  # deterministic predictions of model without extra dropout layer
  freq_classes_no_dropout = predict_classes(model_no_droput, x_all[ix_test_sub, , , , drop = F], 
                                            batch_size = 500)
  (accuracy_freq_no_dropout = mean(y_all[ix_test_sub] == freq_classes_no_dropout))
  accs$accuracy_freq_no_dropout[i] = accuracy_freq_no_dropout
  
  # MC predictions of model with extra dropout layer
  MC_samples_with_dropout = get_mc_predictions(model_with_droput, x_all[ix_test_sub, , , , drop = F], 
                                               nb_iter = 50, batch_size = 500)
  MC_class_preds_with_dropout = predict_MC(MC_samples_with_dropout)
  (accuracy_MC_with_dropout = mean(y_all[ix_test_sub] == MC_class_preds_with_dropout))
  accs$accuracy_MC_with_dropout[i] = accuracy_MC_with_dropout
  
  # MC predictions of model without extra dropout layer
  MC_samples_no_dropout = get_mc_predictions(model_no_droput, x_all[ix_test_sub, , , , drop = F], 
                                             nb_iter = 50, batch_size = 500)
  MC_class_preds_no_dropout = predict_MC(MC_samples_no_dropout)
  (accuracy_MC_no_dropout = mean(y_all[ix_test_sub] == MC_class_preds_no_dropout))
  accs$accuracy_MC_no_dropout[i] = accuracy_MC_no_dropout
  
}



accs %>% 
  gather(key, value, 2:5) %>% 
  ggplot() +
  geom_boxplot(aes(x = key, y = value))


accs %>% 
  gather(key, value, 2:5) %>% 
  ggplot() +
  geom_point(aes(x = key, y = value))