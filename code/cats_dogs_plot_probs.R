library(reticulate)
library(keras)
library(tidyverse)

theme_set(theme_bw())

cats_dogs <- readRDS("../out/cats_dogs/dat_train_64x64.rds")

n_pics = nrow(cats_dogs$x)

set.seed(2018)
ix_train = sample(1:n_pics, size = 20000)
ix_test = setdiff(1:n_pics, ix_train)

x_all <- cats_dogs$x[ix_train,,,]
y_all <- ifelse(cats_dogs$y[ix_train] == "dog", 1, 0)
x_test <- cats_dogs$x[ix_test,,,]
y_test <- ifelse(cats_dogs$y[ix_test] == "dog", 1, 0)

rm(cats_dogs)
gc()


folder_name = "../out/cats_dogs/random_acq/"

model_filenames = grep("model", list.files(folder_name), value = T)


random_probs_filename = "probs_iter_random.rds"

if(exists(paste0("../out/cats_dogs/", random_probs_filename))){
  probs_iter = readRDS(paste0("../out/cats_dogs/", random_probs_filename))
} else {
  probs_iter = map_df(1:50, function(i){
    cat("Iter:", i)
    fn = model_filenames[i]
    cat("\n\tLoading model", fn, "...")
    model = load_model_hdf5(paste0(folder_name, fn))
    cat("loaded.\n\tMaking predictions...")
    out = predict(model, x_test, batch_size = 256) %>% 
      as_tibble() %>% 
      set_names(c("p1", "p2")) %>% 
      mutate(iter = as.integer(i))
    cat("predictions ready.\n\n")
    return(out)
  })
  
  saveRDS(probs_iter, paste0("../out/cats_dogs/", random_probs_filename))  
}

max_probs = apply(probs_iter[,1:2], 1, max)

probs_iter %>% 
  mutate(max_prob = max_probs) %>% 
  ggplot() +
  geom_boxplot(aes(x = iter, y = max_prob, group = iter))



