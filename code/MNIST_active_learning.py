# Bayesian convnet on MNIST dataset
# Example based on Yarin Gal's code https://github.com/yaringal/acquisition_example

# Compute different acquisition functions to be used for active learning

#from __future__ import print_function
import keras
from keras.datasets import mnist
from keras.models import Sequential
from keras.layers import Dense, Dropout, Flatten
from keras.layers import Conv2D, MaxPooling2D
from keras import backend as K
import numpy as np
import os
from utils import get_mc_predictions, predictive_entropy

batch_size = 128
num_classes = 10
#epochs = 12
epochs = 1

# input image dimensions
img_rows, img_cols = 28, 28

# Data preprocessing

# the data, shuffled and split between train and test sets
(x_pool, y_pool), (x_test, y_test) = mnist.load_data()

if K.image_data_format() == 'channels_first':
    x_pool = x_pool.reshape(x_pool.shape[0], 1, img_rows, img_cols)
    x_test = x_test.reshape(x_test.shape[0], 1, img_rows, img_cols)
    input_shape = (1, img_rows, img_cols)
else:
    x_pool = x_pool.reshape(x_pool.shape[0], img_rows, img_cols, 1)
    x_test = x_test.reshape(x_test.shape[0], img_rows, img_cols, 1)
    input_shape = (img_rows, img_cols, 1)

x_pool = x_pool.astype('float32')/255
x_test = x_test.astype('float32')/255
#x_pool /= 255
#x_test /= 255


# convert class vectors to binary class matrices
y_pool = keras.utils.to_categorical(y_pool, num_classes)
y_test = keras.utils.to_categorical(y_test, num_classes)

# Specify architecture
model = Sequential()
model.add(Conv2D(32, kernel_size=(3, 3),
                 activation='relu',
                 input_shape=input_shape))
model.add(Conv2D(64, (3, 3), activation='relu'))
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.25))
model.add(Flatten())
model.add(Dense(128, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(num_classes, activation='softmax'))

model.compile(loss=keras.losses.categorical_crossentropy,
              optimizer=keras.optimizers.Adadelta(),
              metrics=['accuracy'])

# Random initial set of 20 points for training, 100 for validation and the rest as pooling set
indices = np.arange(0, x_pool.shape[0])
np.random.seed(2018)
shuffled_indices = np.random.permutation(indices)
ix_train = shuffled_indices[0:20]
ix_val = shuffled_indices[20:120]
ix_pool = shuffled_indices[120:]
x_val = x_pool[ix_val, :, :]
y_val = y_pool[ix_val]


#for i in xrange(100):
for i in xrange(3):
    x_train = x_pool[ix_train, :, :]
    y_train = y_pool[ix_train]
    x_pool = x_pool[ix_pool, :, :]
    y_pool = y_pool[ix_pool]
    
    model.fit(x_train, y_train,
          batch_size=batch_size,
          epochs=epochs,
          verbose=1,
          validation_data=(x_val, y_val))    

    model.save('../out/MNIST_model_' + str(i) + '.h5')
    
    # nb_MC_samples = 100
    nb_MC_samples = 2

    MNIST_samples_file_name = "../out/MNIST_samples_" + str(i) + ".npy"
    if os.path.exists(MNIST_samples_file_name):
        # file exists
        MC_samples = np.load(MNIST_samples_file_name)
    else:
        MC_samples = get_mc_predictions(model, x_pool, nb_iter=nb_MC_samples, batch_size=256)
        np.save(MNIST_samples_file_name, MC_samples)
    
    pred_entropy = predictive_entropy(MC_samples)
    id_highest_uncertainty = pred_entropy.argsort()[::-1][:10]  # get the 10 points with highest entropy value
    
    ix_train = np.concatenate((ix_train, id_highest_uncertainty))
    ix_pool = np.setdiff1d(ix_pool, id_highest_uncertainty)
    
#    for ind in pred_entropy.argsort()[::-1][:10]:  # get the 10 points with highest entropy value
#        probs_ind = np.mean(MC_samples[:,ind,:], axis = 0)
#        pred_ind = np.argmax(probs_ind)
#        real_ind = np.argmax(y_test[ind])
#        print('index: ', ind, ', acq value: ', pred_entropy[ind], 'prediction: ', pred_ind, 'real: ', real_ind)
#        pylab.imshow(x_test[ind].squeeze(), cmap="gray")
#        pylab.show()






