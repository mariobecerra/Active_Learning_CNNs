library(stringi)
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


# folder_name = "../out/cats_dogs/random_acq/"
# 
# model_filenames = grep("model", list.files(folder_name), value = T)
# 
# 
# probs_filename = "probs_iter_random.rds"
# 
# if(exists(paste0("../out/cats_dogs/", probs_filename))){
#   probs_iter = readRDS(paste0("../out/cats_dogs/", probs_filename))
# } else {
#   probs_iter = map_df(1:50, function(i){
#     cat("Iter:", i)
#     fn = model_filenames[i]
#     cat("\n\tLoading model", fn, "...")
#     model = load_model_hdf5(paste0(folder_name, fn))
#     cat("loaded.\n\tMaking predictions...")
#     out = predict(model, x_test, batch_size = 256) %>% 
#       as_tibble() %>% 
#       set_names(c("p1", "p2")) %>% 
#       mutate(iter = as.integer(i))
#     cat("predictions ready.\n\n")
#     return(out)
#   })
#   
#   saveRDS(probs_iter, paste0("../out/cats_dogs/", probs_filename))  
# }


folder_names = c("../out/cats_dogs/random_acq/",
                 # "../out/cats_dogs/var_ratios/",
                 # "../out/cats_dogs/bald/",
                 # "../out/cats_dogs/predictive_entropy/",
                 "../out/cats_dogs/freq_var_ratios/",
                 "../out/cats_dogs/freq_predictive_entropy/")



for(folder_name in folder_names){
  aux_filename = stri_replace_first(fixed = "../out/cats_dogs/", replacement = "", str = folder_name) %>% 
    stri_replace_first(fixed = "/", replacement = "", str = .)
  
  cat("Acquisition function folder:", aux_filename, "\n")
  
  probs_filename = paste0("probs_iter_", aux_filename, ".rds")
  
  if(file.exists(paste0("../out/cats_dogs/", probs_filename))){
    # probs_iter = readRDS(paste0("../out/cats_dogs/", probs_filename))
    cat("\tFile exists.\n\n")
  } else {
    model_filenames = grep("model", list.files(folder_name), value = T)
    
    n_filenames = length(model_filenames)
    
    probs_iter = map_df(1:n_filenames, function(i){
      cat("\tIter:", i, "of", n_filenames)
      fn = model_filenames[i]
      cat("\n\t\tLoading model", fn, "...")
      model = load_model_hdf5(paste0(folder_name, fn))
      cat("loaded.\n\t\tMaking predictions...")
      out = predict(model, x_test, batch_size = 256) %>% 
        as_tibble() %>% 
        set_names(c("p1", "p2")) %>% 
        mutate(iter = as.integer(i))
      cat("predictions ready.\n\n")
      return(out)
    })
    saveRDS(probs_iter, paste0("../out/cats_dogs/", probs_filename))  
  }
}




max_probs = apply(probs_iter_random[,1:2], 1, max)

probs_iter_random %>% 
  mutate(max_prob = max_probs) %>% 
  group_by(iter) %>% 
  summarize(prob_q10 = quantile(max_prob, 0.1),
            median = median(max_prob),
            prob_q90 = quantile(max_prob, 0.9)) %>% 
  ggplot() +
  geom_hline(yintercept = 0.5) +
  geom_errorbar(aes(x = iter, 
                    ymin = prob_q10, 
                    ymax = prob_q90), 
                width = 0.4, size = 0.3) + 
  geom_point(aes(x = iter, y = median),
             size = 0.7) +
  ylab("prob")