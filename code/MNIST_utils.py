# def create_shuffled_indices_MNIST(seed = 2018, nrow = 60000):
#   seed = int(seed)
#   nrow = int(nrow)
#   indices = np.arange(0, nrow)
#   np.random.seed(seed)
#   shuffled_indices = np.random.permutation(indices)
#   return shuffled_indices

def MNIST_model(n_train):
  img_rows, img_cols = 28, 28
  if K.image_data_format() == 'channels_first':
    input_shape = (1, img_rows, img_cols)
  else:
    input_shape = (img_rows, img_cols, 1)
  
  # number of convolutional filters to use
  nb_filters = 32
  # size of pooling area for max pooling
  nb_pool = 2
  # convolution kernel size
  nb_conv = 4

  nb_classes = 10

  c = 2.5
  Weight_Decay = c / float(n_train)

  # Specify architecture
  model = Sequential()
  model.add(Conv2D(nb_filters, kernel_size=(nb_conv, nb_conv),
                   activation='relu',
                   input_shape=input_shape))
  model.add(Conv2D(nb_filters, (nb_conv, nb_conv), activation='relu'))
  model.add(MaxPooling2D(pool_size=(nb_pool, nb_pool)))
  model.add(Dropout(0.25))
  model.add(Flatten())
  model.add(Dense(128, W_regularizer=l2(Weight_Decay)))
  model.add(Activation('relu'))
  model.add(Dropout(0.5))
  model.add(Dense(nb_classes))
  model.add(Activation('softmax'))
  model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
  
  print("Compiled model")
  return(model)


def MNIST_model_deeper(n_train):
  img_rows, img_cols = 28, 28
  # number of convolutional filters to use
  nb_filters = 32
  # size of pooling area for max pooling
  nb_pool = 2
  # convolution kernel size
  nb_conv = 3

  nb_classes = 10

  # specify input shape
  if K.image_data_format() == 'channels_first':
    input_shape = (1, img_rows, img_cols)
  else:
    input_shape = (img_rows, img_cols, 1)
  
  

  # Specify architecture
  model = Sequential()
  model.add(Conv2D(nb_filters, nb_conv, nb_conv, border_mode='valid', input_shape = input_shape))
  model.add(Activation('relu'))
  model.add(Conv2D(nb_filters, nb_conv, nb_conv))
  model.add(Activation('relu'))
  model.add(MaxPooling2D(pool_size=(nb_pool, nb_pool)))
  model.add(Dropout(0.25))

  model.add(Conv2D(nb_filters*2, nb_conv, nb_conv, border_mode='valid', input_shape = input_shape))
  model.add(Activation('relu'))
  model.add(Conv2D(nb_filters*2, nb_conv, nb_conv))
  model.add(Activation('relu'))
  model.add(MaxPooling2D(pool_size=(nb_pool, nb_pool)))
  model.add(Dropout(0.25))

  c = 2.5
  Weight_Decay = c / float(n_train)
  model.add(Flatten())
  model.add(Dense(128, W_regularizer=l2(Weight_Decay)))
  model.add(Activation('relu'))
  model.add(Dropout(0.5))
  model.add(Dense(nb_classes))
  model.add(Activation('softmax'))

  # compile model
  model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
  
  print("Compiled model")
  return(model)
