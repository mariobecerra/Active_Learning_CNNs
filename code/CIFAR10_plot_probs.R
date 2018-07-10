library(stringi)
library(reticulate)
library(keras)
library(tidyverse)

theme_set(theme_bw())


folder_names = c("../out/CIFAR10/random_acq/",
                 # "../out/CIFAR10/var_ratios/",
                 # "../out/CIFAR10/bald/",
                 # "../out/CIFAR10/predictive_entropy/",
                 "../out/CIFAR10/freq_var_ratios/",
                 "../out/CIFAR10/freq_predictive_entropy/")





for(folder_name in folder_names){
  aux_filename = stri_replace_first(fixed = "../out/CIFAR10/", replacement = "", str = folder_name) %>% 
    stri_replace_first(fixed = "/", replacement = "", str = .)
  
  cat("Acquisition function folder:", aux_filename, "\n")
  
  probs_filename = paste0("probs_iter_", aux_filename, ".rds")
  
  if(file.exists(paste0("../out/CIFAR10/", probs_filename))){
    # probs_iter = readRDS(paste0("../out/CIFAR10/", probs_filename))
    cat("\tFile exists.\n\n")
  } else {
    if(!exists("x_test")){
      # Load data if it doesn't exist
      CIFAR10 <- readRDS("../out/cifar10.rds")
      
      # x_all <- CIFAR10$train$x
      # y_all <- as.integer(CIFAR10$train$y)
      x_test <- CIFAR10$test$x/255
      # y_test <- as.integer(CIFAR10$test$y)

      gc()
    }
    
    
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
    saveRDS(probs_iter, paste0("../out/CIFAR10/", probs_filename))  
  }
}





dat_probs = map_df(folder_names, function(folder_name){
  aux_filename = stri_replace_first(fixed = "../out/CIFAR10/", replacement = "", str = folder_name) %>% 
    stri_replace_first(fixed = "/", replacement = "", str = .)
  
  cat("Acquisition function folder:", aux_filename, "\n")
  
  probs_filename = paste0("probs_iter_", aux_filename, ".rds")
  df_temp = readRDS(paste0("../out/CIFAR10/", probs_filename)) %>% 
    mutate(acq_func = aux_filename,
           max_prob = pmax(p1, p2))
  
  return(df_temp)
})


dat_probs %>% 
  group_by(iter, acq_func) %>% 
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
  ylab("prob") +
  facet_wrap(~acq_func)