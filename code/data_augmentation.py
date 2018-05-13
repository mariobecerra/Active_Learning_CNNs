from keras.preprocessing.image import ImageDataGenerator, array_to_img, img_to_array, load_img

files = list()
files.append("../data/earthquake/024f7a22-9561-49ae-a168-e9b200c6b40d.jpg")
files.append("../data/earthquake/11f1a8c3-f2c4-4c40-b8d1-462bb96aee34.jpg")
files.append("../data/earthquake/008c4985-46db-4aaf-88b6-65373e37304b.jpg")



datagen = ImageDataGenerator(
        rotation_range=180,
        width_shift_range=0.1,
        height_shift_range=0.1,
        shear_range=0,
        zoom_range=0.2,
        horizontal_flip=True,
        fill_mode='reflect')


for file_name in files:
    img = load_img(file_name)
    x = img_to_array(img)  # this is a Numpy array with shape (3, 150, 150)
    x = x.reshape((1,) + x.shape)  # this is a Numpy array with shape (1, 3, 150, 150)

    i = 0
    for batch in datagen.flow(x, batch_size=1,
                          save_to_dir='../out/augmented_data/', save_prefix='a01', save_format='jpeg'):
        i += 1
        if i >= 10:
            break  # otherwise the generator would loop indefinitely
        

    

