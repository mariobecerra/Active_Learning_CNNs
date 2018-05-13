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

def MNIST_model_extra_dropout(n_train):
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
    model.add(Dropout(0.5))
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
