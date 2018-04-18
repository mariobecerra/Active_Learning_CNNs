# Bayesian convnet on MNIST dataset
# Example based on Yarin Gal's code https://github.com/yaringal/acquisition_example

#  Active Learning with bayesian convnet and MNIST dataset

#from __future__ import print_function
import keras
from keras.datasets import mnist
from keras.models import Sequential
from keras.layers import Dense, Dropout, Flatten
from keras.layers import Conv2D, MaxPooling2D
from keras import backend as K
import numpy as np
import os
from utils import get_mc_predictions, predictive_entropy, BALD, variation_ratios, predict_MC
#exec(open("utils.py").read())

batch_size = 128
num_classes = 10
epochs = 50
#epochs = 1

# input image dimensions
img_rows, img_cols = 28, 28

# Data preprocessing

# the data, shuffled and split between train and test sets
(x_all, y_all), (x_test, y_test) = mnist.load_data()
print("Loaded MNIST data")

if K.image_data_format() == 'channels_first':
    x_all = x_all.reshape(x_all.shape[0], 1, img_rows, img_cols)
    x_test = x_test.reshape(x_test.shape[0], 1, img_rows, img_cols)
    input_shape = (1, img_rows, img_cols)
else:
    x_all = x_all.reshape(x_all.shape[0], img_rows, img_cols, 1)
    x_test = x_test.reshape(x_test.shape[0], img_rows, img_cols, 1)
    input_shape = (img_rows, img_cols, 1)

x_all = x_all.astype('float32')/255
x_test = x_test.astype('float32')/255
#x_all /= 255
#x_test /= 255


# convert class vectors to binary class matrices
y_all_cat = keras.utils.to_categorical(y_all, num_classes)
y_test_cat = keras.utils.to_categorical(y_test, num_classes)

print("Converted class vectors")

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

print("Compiled model")

# Random initial set of 20 points for training, 100 for validation and the rest as pooling set
indices = np.arange(0, x_all.shape[0])
np.random.seed(2018)
shuffled_indices = np.random.permutation(indices)
ix_train = shuffled_indices[0:20]
ix_val = shuffled_indices[20:120]
ix_pool = shuffled_indices[120:]
x_val = x_all[ix_val, :, :]
y_val = y_all_cat[ix_val]






n_acq_steps = 100
#n_acq_steps = 3
#acq_fun_string = ['predictive_entropy', 'var_ratios', 'bald']
acq_fun_string = ['var_ratios', 'bald']

accuracies = np.zeros(shape = (n_acq_steps*3, 2))
j_acc = -1

#acq_fun_int = 0 # keeps track of acquisition function index
acq_fun_int = 1 # 1 when starting with var ratios
for acq_fun in acq_fun_string:
    print("\tAcquisition function: " + acq_fun)
    acq_fun_int += 1
    for i in range(n_acq_steps):
        j_acc += 1
        print("\t\tIter: " + str(i))
        x_train = x_all[ix_train, :, :]
        y_train = y_all_cat[ix_train]
    #    x_all = x_all[ix_pool, :, :]
    #    y_all = y_all[ix_pool]
        
        model_file_name = '../out/MNIST_model_' + acq_fun + "_" + str(i) + '.h5'
        if os.path.exists(model_file_name):
            # file exists
            print("\t\t\tLoading model from " + model_file_name)
            model = keras.models.load_model(model_file_name)
        else:
            model.fit(x_train, y_train,
              batch_size=batch_size,
              epochs=epochs,
              verbose=1,
              validation_data=(x_val, y_val))
            print("\t\t\tSaving model to " + model_file_name)
            model.save(model_file_name)
                    
        nb_MC_samples = 100
        # nb_MC_samples = 2

        MNIST_samples_file_name = "../out/MNIST_samples_" + acq_fun + "_" + str(i) + ".npy"
        if os.path.exists(MNIST_samples_file_name):
            print("\t\t\tLoading pool samples from " + MNIST_samples_file_name)
            MC_samples = np.load(MNIST_samples_file_name)
        else:
            print("\t\t\tComputing pool samples")
            MC_samples = get_mc_predictions(model, x_all[ix_pool,:,:], nb_iter=nb_MC_samples, batch_size=256)
            print("\t\t\tSaving pool samples to " + MNIST_samples_file_name)
            np.save(MNIST_samples_file_name, MC_samples)
        
        if acq_fun == 'predictive_entropy':
            acq_func_values = predictive_entropy(MC_samples)
        if acq_fun == 'var_ratios':
            acq_func_values = variation_ratios(MC_samples)
        if acq_fun == 'bald':
            acq_func_values = BALD(MC_samples)
        
        id_highest_uncertainty = acq_func_values.argsort()[::-1][:10]  # get the 10 points with highest entropy value
        
        ix_train = np.concatenate((ix_train, id_highest_uncertainty))
        ix_pool = np.setdiff1d(ix_pool, id_highest_uncertainty)
        
        MNIST_samples_test_file_name = "../out/MNIST_samples_test_" + acq_fun + "_" + str(i) + ".npy"
        if os.path.exists(MNIST_samples_test_file_name):
            # file exists
            print("\t\t\tLoading test samples from " + MNIST_samples_test_file_name)
            MC_samples_test = np.load(MNIST_samples_test_file_name)
        else:
            print("\t\t\tComputing test samples")
            MC_samples_test = get_mc_predictions(model, x_test, nb_iter=nb_MC_samples, batch_size=256)
            print("\t\t\tSaving test samples to " + MNIST_samples_test_file_name)
            np.save(MNIST_samples_test_file_name, MC_samples_test)
        
        test_preds = predict_MC(MC_samples_test)
        
        accuracy = np.mean(y_test == test_preds)
        accuracies[j_acc,:] = [acq_fun_int, accuracy]
        print("\t\t\tAccuracy computed\n\n\n")
        print("\t\t\tSaving accuracies so far...")
        np.save("../out/MNIST_accuracies_so_far.npy", accuracies)
        print("\t\t\tAccuracies saved.")

    #    for ind in pred_entropy.argsort()[::-1][:10]:  # get the 10 points with highest entropy value
    #        probs_ind = np.mean(MC_samples[:,ind,:], axis = 0)
    #        pred_ind = np.argmax(probs_ind)
    #        real_ind = np.argmax(y_test[ind])
    #        print('index: ', ind, ', acq value: ', pred_entropy[ind], 'prediction: ', pred_ind, 'real: ', real_ind)
    #        pylab.imshow(x_test[ind].squeeze(), cmap="gray")
    #        pylab.show()

print("Loop ended.\n\n")

print("Saving final accuracies...")
np.save("../out/MNIST_accuracies.npy", accuracies)
print("Final ccuracies saved.")
