library(Rfast)
library(stringi)
library(tidyverse)
library(here)

theme_set(theme_bw())
out_out = here("out/")


folders = c(paste0(out_out, "CIFAR10/"),
            paste0(out_out, "MNIST/"),
            paste0(out_out, "cats_dogs/"))


rds_files = map(folders, function(fldr) {
  files_out = list.files(fldr)
  probs_files = grep("probs_iter_", files_out, fixed = T, value = T)
  out = paste0(fldr, probs_files)
  return(out)
}) %>% 
  set_names(folders)




dat_probs_list = map(seq_along(rds_files), function(i){
  short_route = stri_extract_first(regex = "out/.*", names(rds_files)[i])
  dataset_name = gsub("out/|/", "", short_route)
  file_names = rds_files[[i]]
  acq_functions = gsub(".*/out/", "", file_names) %>% 
    stri_replace_first(fixed = paste0(dataset_name, "/"), replacement = "", str = .) %>% 
    stri_replace_first(fixed = "probs_iter_", replacement = "", str = .) %>% 
    stri_replace_first(regex = "_2018.*", replacement = "", str = .) 
  runs_id = tibble(acq_funct = acq_functions) %>% 
    group_by(acq_funct) %>% 
    mutate(run = 1:n()) %>% 
    ungroup()
  df_temp_out = map_df(seq_along(file_names), function(j){
    file_name_rds = file_names[j]
    print(file_name_rds)
    
    df_temp = readRDS(file_name_rds)
    
    max_values = rowMaxs(as.matrix(select(df_temp, -iter)), value = TRUE)
    
    df_temp = df_temp %>% 
      mutate(max_prob = max_values, 
             acq_func = runs_id$acq_funct[j],
             run = runs_id$run[j])
    return(df_temp)
  }) %>% 
    mutate(dataset = dataset_name)
  return(df_temp_out)
}) 



dat_plots = map_df(dat_probs_list, function(dat){
  dat_out = dat %>% 
    select(iter, run, dataset, acq_func, max_prob)
  return(dat_out)
}) 



dat_plots %>% 
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
  facet_wrap(dataset~acq_func, scales = "free_x", ncol = 3)

