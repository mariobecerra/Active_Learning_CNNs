library(Rfast)
library(scales)
library(stringi)
library(tidyverse)
library(here)

theme_set(theme_bw())
out_out = here("out/")


names_acq = tibble(
  acq_func_name = c("BALD", "Frequentist predictive entropy", "Frequentist variation ratios",
                    "Predictive entropy", "Random", "Variation ratios"),
  acq_func = c("bald", "freq_predictive_entropy", "freq_var_ratios",
               "predictive_entropy", "random", "var_ratios"))

plot_data_filename = here("out/probs_plot_data.rds")

if(file.exists(plot_data_filename)){
  dat_plots = readRDS(plot_data_filename)
} else{
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
  
  saveRDS(dat_plots, plot_data_filename)
}

min_prob = min(dat_quantiles$prob_q10)



plot_probs = function(dataset, min_y_axis){
  min_y_axis = floor(min_prob*20)/20
  
  out = dataset %>% 
    left_join(names_acq) %>% 
    ggplot(aes(x = num_images)) +
    geom_errorbar(aes(ymin = prob_q10, 
                      ymax = prob_q90), 
                  width = 0.4, size = 0.3) + 
    geom_point(aes(y = median),
               size = 0.7) +
    xlab("Number of images") +
    ylab("Predicted probability on test set") +
    scale_x_continuous(label = comma) +
    ylim(min_y_axis, 1) +
    facet_wrap(~acq_func_name, scales = "free_x", ncol = 3) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(out)
}




dat_quantiles = dat_plots %>% 
  group_by(dataset, acq_func, iter) %>% 
  summarize(prob_q10 = quantile(max_prob, 0.1),
            median = median(max_prob),
            prob_q90 = quantile(max_prob, 0.9))


### Final plots


plot_mnist = dat_quantiles %>% 
  filter(dataset == "MNIST") %>% 
  mutate(num_images = 20 + (iter-1)*10) %>% 
  plot_probs(., min_prob)




plot_cifar10 = dat_quantiles %>% 
  filter(dataset == "CIFAR10") %>% 
  mutate(num_images = 100 + (iter-1)*1000) %>% 
  plot_probs(., min_prob)




plot_cats_dogs = dat_quantiles %>% 
  filter(dataset == "cats_dogs") %>% 
  mutate(num_images = 100 + (iter-1)*50) %>% 
  plot_probs(., min_prob)


# Plot file dimensions
out_width = 17
out_height = 7
out_units = "cm"
out_dpi = 300


ggsave(plot = plot_mnist, 
       file = here("out/MNIST/plots/MNIST_probs_plot.png"), 
       device = "png",
       width = out_width, 
       height = out_height,
       units = out_units,
       dpi = out_dpi)


ggsave(plot = plot_cifar10, 
       file = here("out/CIFAR10/plots/CIFAR10_probs_plot.png"), 
       device = "png",
       width = out_width, 
       height = out_height,
       units = out_units,
       dpi = out_dpi)


ggsave(plot = plot_cats_dogs, 
       file = here("out/cats_dogs/plots/cats_dogs_probs_plot.png"), 
       device = "png",
       width = out_width, 
       height = out_height,
       units = out_units,
       dpi = out_dpi)


