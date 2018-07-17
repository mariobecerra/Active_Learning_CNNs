library(stringi)
library(tidyverse)

theme_set(theme_bw())


folder_names_CIFAR10 = c("../out/CIFAR10/random_acq/",
                         # "../out/CIFAR10/var_ratios/",
                         # "../out/CIFAR10/bald/",
                         # "../out/CIFAR10/predictive_entropy/",
                         "../out/CIFAR10/freq_var_ratios/",
                         "../out/CIFAR10/freq_predictive_entropy/")

folder_names_cats_dogs = c("../out/cats_dogs/random_acq/",
                           # "../out/cats_dogs/var_ratios/",
                           # "../out/cats_dogs/bald/",
                           # "../out/cats_dogs/predictive_entropy/",
                           "../out/cats_dogs/freq_var_ratios/",
                           "../out/cats_dogs/freq_predictive_entropy/")

folder_names_MNIST = c("../out/MNIST/random_acq/",
                       # "../out/MNIST/var_ratios/",
                       # "../out/MNIST/bald/",
                       # "../out/MNIST/predictive_entropy/",
                       "../out/MNIST/freq_var_ratios/",
                       "../out/MNIST/freq_predictive_entropy/")


folder_names = list(folder_names_cats_dogs, folder_names_CIFAR10, folder_names_MNIST)


dat_probs = map(folder_names, function(aaaa){
  df_temp_outer = map_df(aaaa, function(folder_name){
    dataset_name = stri_replace_all(str = substr(folder_name, start = 8, stop = nchar(folder_name)), 
                                    regex = "\\/.*", 
                                    replacement = "")
    
    cat("\n\nDataset:", dataset_name, "\n")
    
    aux_filename = stri_replace_first(fixed = paste0("../out/", dataset_name, "/"), replacement = "", str = folder_name) %>% 
      stri_replace_first(fixed = "/", replacement = "", str = .)
    
    cat("\tAcquisition function folder:", aux_filename, "\n")
    
    probs_filename = paste0("probs_iter_", aux_filename, ".rds")
    df_temp = readRDS(paste0("../out/", dataset_name, "/", probs_filename))
    max_values = apply(select(df_temp, starts_with("p")), 1, max)
    df_out = df_temp %>% 
      mutate(max_prob = max_values,
             acq_func = aux_filename,
             dataset = dataset_name)
    
    return(df_out)
  })
  return(df_temp_outer)
})
  
  



dat_probs %>% 
  group_by(dataset, acq_func, iter) %>% 
  summarize(prob_q10 = quantile(max_prob, 0.1),
            median = median(max_prob),
            prob_q90 = quantile(max_prob, 0.9)) %>% 
  ggplot() +
  geom_hline(yintercept = 0.1) +
  geom_errorbar(aes(x = iter, 
                    ymin = prob_q10, 
                    ymax = prob_q90), 
                width = 0.4, size = 0.3) + 
  geom_point(aes(x = iter, y = median),
             size = 0.7) +
  ylab("prob") +
  facet_grid(dataset~acq_func)
