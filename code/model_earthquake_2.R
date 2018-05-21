library(keras)
library(tidyverse)


train_samples = 200
validation_samples = 200
batch_size = 32
img_height = 322
img_width = 322

# base_model = application_inception_v3(
#   input_shape = c(322, 322, 3),
#   weights = 'imagenet', 
#   include_top = FALSE)

# base_model = application_mobilenet(
#   input_shape = c(img_width, img_height, 3),
#   weights = 'imagenet', 
#   include_top = FALSE)

base_model <- application_vgg16(
  input_shape = c(img_width, img_height, 3),
  weights = 'imagenet',
  include_top = FALSE)

# add our custom layers
predictions <- base_model$output %>% 
  layer_global_average_pooling_2d(trainable = T) %>% 
  layer_dense(64, trainable = T) %>%
  layer_activation("relu", trainable = T) %>%
  layer_dropout(0.4, trainable = T) %>%
  layer_dense(2, trainable = T) %>%    ## important to adapt to fit the 2 classes in the dataset!
  layer_activation("softmax", trainable = T)

# this is the model we will train
model <- keras_model(inputs = base_model$input, outputs = predictions)

for (layer in base_model$layers)
  layer$trainable <- FALSE


model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.003, decay = 1e-6),  ## play with the learning rate
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


hist = model %>% 
  fit_generator(
    train_generator,
    steps_per_epoch = as.integer(train_samples/batch_size),
    epochs = 10,
    validation_data = validation_generator,
    validation_steps = as.integer(validation_samples/batch_size),
    verbose = 2)


### saveable data frame obejct.
histDF <- data.frame(acc = unlist(hist$history$acc), 
                     val_acc=unlist(hist$history$val_acc), 
                     val_loss = unlist(hist$history$val_loss),
                     loss = unlist(hist$history$loss))
