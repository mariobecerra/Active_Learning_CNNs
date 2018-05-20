library(keras)
library(tidyverse)

model <- keras_model_sequential()
model %>%
  layer_conv_2d(filter = 6,
                kernel_size = c(3, 3),
                padding = "same",
                input_shape = c(322, 322, 3)) %>%
  layer_activation("relu") %>%
  layer_conv_2d(filter = 16, kernel_size = c(6, 6))  %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.5) %>%
  layer_flatten() %>%
  layer_dense(64) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(2) %>%
  layer_activation("softmax")

# model <- keras_model_sequential()
# model %>%  
#   #defining a 2-D convolution layer
#   layer_conv_2d(filter=32,kernel_size=c(3,3),padding="same",                
#                 input_shape=c(256,256,3) ) %>%  
#   layer_activation("relu") %>%  
#   #another 2-D convolution layer
#   layer_conv_2d(filter=32 ,kernel_size=c(3,3))  %>%  
#   layer_activation("relu") %>%
#   #Defining a Pooling layer which reduces the dimentions of the 
#   #features map and reduces the computational complexity of the model
#   layer_max_pooling_2d(pool_size=c(2,2)) %>%  
#   #dropout layer to avoid overfitting
#   layer_dropout(0.25) %>%
#   layer_conv_2d(filter=32 , kernel_size=c(3,3),padding="same") %>% 
#   layer_activation("relu") %>%  layer_conv_2d(filter=32,kernel_size=c(3,3) ) %>%  
#   layer_activation("relu") %>%  
#   layer_max_pooling_2d(pool_size=c(2,2)) %>%  
#   layer_dropout(0.25) %>%
#   #flatten the input  
#   layer_flatten() %>%  
#   layer_dense(512) %>%  
#   layer_activation("relu") %>%  
#   layer_dropout(0.5) %>%  
#   #output layer-10 classes-10 units  
#   layer_dense(2) %>%  
#   #applying softmax nonlinear activation function to the output layer #to calculate cross-entropy  
#   layer_activation("softmax") 


opt <- optimizer_adam(lr= 0.0001, decay = 1e-6 )

model %>%
  compile(loss="binary_crossentropy",
          optimizer=opt,
          metrics = "accuracy")
#Summary of the Model and its Architecture
summary(model)

# gen_images <- image_data_generator(rescale=1./255,
#                                    data_format = "channels_last")


model %>% 
  fit_generator(
    flow_images_from_directory("../out/earthquake_folders_data/juchitan_de_zaragoza/",
                               generator = image_data_generator(rescale=1./255),
                               target_size = c(322, 322),
                               batch_size = 32),
    steps_per_epoch = as.integer(200/32),
    epochs = 10,
    validation_data = flow_images_from_directory("../out/earthquake_folders_data/santa_maria_xadani/",
                                                 generator = image_data_generator(rescale=1./255),
                                                 target_size = c(322, 322),
                                                 batch_size = 32))

 
# See here: https://keras.io/models/sequential/62
# 
# “steps: Total number of steps (batches of samples) to yield from generator before stopping.”
# 
# What this general means for Keras 2 is that steps should be equal to the number of training examples divided by your batch size (trn.n/batch_size). What you are defining is how many batches you are going to predict.
# 
# If you have steps=30 and your batch size = 4, you will predict the results for only 120 examples.

preds = predict_generator(model,
                  flow_images_from_directory("../out/earthquake_folders_data/santa_maria_xadani/",
                                             generator = image_data_generator(rescale=1./255),
                                             target_size = c(322, 322),
                                             batch_size = 20),
                  steps = 10,
                  verbose = 1)

aaa = evaluate_generator(model,
                   flow_images_from_directory("../out/earthquake_folders_data/union_hidalgo/",
                                              generator = image_data_generator(rescale=1./255),
                                              target_size = c(322, 322),
                                              batch_size = 20),
                   steps = 10)

aaa_santa_maria = evaluate_generator(model,
                         flow_images_from_directory("../out/earthquake_folders_data/santa_maria_xadani/",
                                                    generator = image_data_generator(rescale=1./255),
                                                    target_size = c(322, 322),
                                                    batch_size = 20),
                         steps = 10)
aaa_santa_maria

bbb = image_to_array(image_load("../out/earthquake_folders_data/union_hidalgo/absent/00ceb2ec-4667-4f95-afe6-52fa39423390.jpg"))

dim(bbb)

predict(model, bbb)

absent_un_h = list.files("../out/earthquake_folders_data/union_hidalgo/absent/")
present_un_h = list.files("../out/earthquake_folders_data/union_hidalgo/present/")

apply(preds, 1, which.max)

save_model_hdf5(model, "../out/earthquake_model_1.rds")



model = load_model_hdf5("../out/earthquake_model_1.rds")
