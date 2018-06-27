library(keras)
library(tidyverse)


train_samples = 200
validation_samples = 200
batch_size = 32
img_height = 224
img_width = 224

# base_model = application_inception_v3(
#   input_shape = c(322, 322, 3),
#   weights = 'imagenet', 
#   include_top = FALSE)

base_model = application_mobilenet(
  input_shape = c(img_width, img_height, 3),
  weights = 'imagenet',
  include_top = FALSE)

# base_model <- application_vgg16(
#   input_shape = c(img_width, img_height, 3),
#   weights = 'imagenet',
#   include_top = FALSE)

# add our custom layers
predictions <- base_model$output %>% 
  layer_global_average_pooling_2d(trainable = T) %>% 
  # layer_dense(32, trainable = T,
  #             activity_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dense(32, trainable = T) %>%
  layer_activation("relu", trainable = T) %>%
  layer_dropout(0.4, trainable = T) %>%
  # layer_dense(16, trainable = T,
  #             activity_regularizer = regularizer_l2(l = 0.01)) %>%
  # layer_activation("relu", trainable = T) %>%
  # layer_dropout(0.5, trainable = T) %>%
  layer_dense(16, trainable = T) %>%
  layer_activation("relu", trainable = T) %>%
  layer_dropout(0.5, trainable = T) %>%
  # layer_dense(2, trainable = T,
  #             activity_regularizer = regularizer_l2(l = 0.01)) %>%
  layer_dense(2, trainable = T) %>%
  layer_activation("softmax", trainable = T)

# this is the model we will train
model <- keras_model(inputs = base_model$input, outputs = predictions)

for (layer in base_model$layers)
  layer$trainable <- FALSE

summary(model)


model %>% 
  compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(lr = 0.003, decay = 1e-6),
    #optimizer = optimizer_sgd(lr = 0.01, momentum = 0.05, decay = 1e-5, nesterov = T),
    metrics = "accuracy"
  )


train_generator = flow_images_from_directory("../out/earthquake_folders_data/juchitan_de_zaragoza/",
                                             generator = image_data_generator(rescale=1./255),
                                             target_size = c(img_width, img_height),
                                             batch_size = batch_size)

validation_generator = flow_images_from_directory("../out/earthquake_folders_data/santa_maria_xadani/",
                                                  generator = image_data_generator(rescale=1./255),
                                                  target_size = c(img_width, img_height),
                                                  batch_size = batch_size)

# hist <- model %>% 
#   fit_generator(
#     train_generator,
#     steps_per_epoch = as.integer(train_samples/batch_size), 
#     epochs = 20, 
#     validation_data = validation_generator,
#     validation_steps = as.integer(validation_samples/batch_size),
#     verbose=2
#   )

time_file = as.character(Sys.time()) %>% 
  stringr::str_replace_all(":", replacement = ".") %>% 
  stringr::str_replace_all(" ", replacement = "_")

hist = model %>% 
  fit_generator(
    train_generator,
    steps_per_epoch = as.integer(train_samples/batch_size),
    epochs = 50,
    validation_data = validation_generator,
    validation_steps = as.integer(validation_samples/batch_size),
    verbose = 2)


# metrics history to tibble
hist_df <- hist$metrics %>% 
  as_tibble() %>% 
  # data.frame(acc = hist$metrics$acc, 
  #                    val_acc = hist$metrics$val_acc, 
  #                    val_loss = hist$metrics$val_loss,
  #                    loss = hist$metrics$loss) %>% 
  mutate(epoch = 1:nrow(.)) %>% 
  select(ncol(.), 1:(ncol(.)- 1)) # reorder so epoch is first column


write_csv(hist_df, paste0("../out/earthquake_models/metrics_history_earthquake_model_", time_file, ".csv"))
keras::save_model_hdf5(model, paste0("../out/earthquake_models/earthquake_model_", time_file, ".hdf5"))

theme_set(theme_bw())

hist_df %>% select(epoch, acc, val_acc) %>% gather(key, value, -epoch) %>% 
  ggplot(aes(epoch, value, color = key)) + geom_point() + geom_line()


bbb = image_to_array(image_load("../out/earthquake_folders_data_resized/union_hidalgo/absent/00ceb2ec-4667-4f95-afe6-52fa39423390.jpg"))

dim(bbb)

bbb2 = array(bbb, dim = c(1, 224, 224, 3))
dim(bbb2)

predict(model, bbb2)

reticulate::source_python("utils.py")

mc_preds_temp = get_mc_predictions(model, bbb2, 40, 10)

predict_MC(mc_preds_temp)
