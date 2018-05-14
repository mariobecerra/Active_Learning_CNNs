library(keras)
library(tidyverse)

model <- keras_model_sequential()
model %>%
  layer_conv_2d(filter = 6,
                kernel_size = c(3, 3),
                padding = "same",
                input_shape = c(256, 256, 3)) %>%
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
                               # gen_images,
                               batch_size = 32),
    steps_per_epoch = as.integer(500/32),
    epochs = 10,
    validation_data = flow_images_from_directory("../out/earthquake_folders_data/santa_maria_xadani/",
                                                 # gen_images,
                                                 batch_size = 32))

# preds = predict_generator(model,
#                   flow_images_from_directory("../out/earthquake_folders_data/union_hidalgo/",
#                                              batch_size = 20),
#                   steps = 12,
#                   verbose = 1)

aaa = evaluate_generator(model,
                   flow_images_from_directory("../out/earthquake_folders_data/union_hidalgo/",
                                              batch_size = 20),
                   steps = 10)

bbb = image_to_array(image_load("../out/earthquake_folders_data/union_hidalgo/absent/00ceb2ec-4667-4f95-afe6-52fa39423390.jpg"))

dim(bbb)



