from __future__ import print_function
import keras
from keras.datasets import mnist
from keras.models import Sequential
from keras.layers import Dense, Dropout, Flatten
from keras.layers import Conv2D, MaxPooling2D
from keras import backend as K
from tqdm import tqdm
import numpy as np

####################################################
## MC predictions (dropout at prediction time)
####################################################

def get_mc_predictions(model, X, nb_iter=50, batch_size=256):
    # https://github.com/rfeinman/detecting-adversarial-samples/blob/ded6630ac30a8ef13bfadb7545e2ca5045988046/detect/util.py
    """
    TODO
    :param model:
    :param X:
    :param nb_iter:
    :param batch_size:
    :return:
    """
    output_dim = model.layers[-1].output.shape[-1].value
    get_output = K.function(
        [model.layers[0].input, K.learning_phase()],
        [model.layers[-1].output]
    )

    def predict():
        n_batches = int(np.ceil(X.shape[0] / float(batch_size)))
        output = np.zeros(shape=(len(X), output_dim))
        for i in range(n_batches):
            output[i * batch_size:(i + 1) * batch_size] = \
                get_output([X[i * batch_size:(i + 1) * batch_size], 1])[0]
        return output

    preds_mc = []
    for i in tqdm(range(nb_iter)):
        preds_mc.append(predict())

    return np.asarray(preds_mc)



####################################################
## Acquisition functions
####################################################

def predictive_entropy(MC_samples):
    "MC_samples: numpy array with MC samples"
    p_y_c = np.mean(MC_samples, axis=0)
    log_p_y_c = np.log(p_y_c)
    pred_entropy = - np.sum(np.log(p_y_c + 1e-10)*p_y_c, axis = 1)
    return pred_entropy



def variation_ratios(MC_samples):
    p_y_c = np.mean(MC_samples, axis=0)
    var_ratios = 1 - np.max(p_y_c, axis = 1)
    return var_ratios



def BALD(MC_samples):
    pred_entropy = predictive_entropy(MC_samples)
    expected_entropy = - np.mean(np.sum(MC_samples * np.log(MC_samples + 1e-10), axis=-1), axis=0)  # [batch size]
    BALD_acq = pred_entropy - expected_entropy
    return BALD_acq



