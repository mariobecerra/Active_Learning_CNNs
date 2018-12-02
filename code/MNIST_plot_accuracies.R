library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ggplot2)

theme_set(theme_bw())

out_dirs = list.dirs("../out/MNIST")


random_dirs = grep("random", out_dirs, ignore.case = T, value = T)

random_accuracies = map_df(seq_along(random_dirs), function(i){
  file_name = grep("accuracies", list.files(random_dirs[i], full.names = T), value = T)
  out = read_csv(file_name) %>% 
    mutate(run_id = i)
})


bald_dirs = grep("bald", out_dirs, ignore.case = T, value = T)

bald_accuracies = map_df(seq_along(bald_dirs), function(i){
  file_name = grep("accuracies", list.files(bald_dirs[i], full.names = T), value = T)
  out = read_csv(file_name) %>% 
    mutate(run_id = i)
})



predictive_entropy_dirs = grep("predictive_entropy", out_dirs, ignore.case = T, value = T) %>% 
  grep("freq", ., ignore.case = T, value = T, invert = T)

predictive_entropy_accuracies = map_df(seq_along(predictive_entropy_dirs), function(i){
  file_name = grep("accuracies", list.files(predictive_entropy_dirs[i], full.names = T), value = T)
  out = read_csv(file_name) %>% 
    mutate(run_id = i)
})



var_ratios_dirs = grep("var_ratios", out_dirs, ignore.case = T, value = T) %>% 
  grep("freq", ., ignore.case = T, value = T, invert = T)

var_ratios_accuracies = map_df(seq_along(var_ratios_dirs), function(i){
  file_name = grep("accuracies", list.files(var_ratios_dirs[i], full.names = T), value = T)
  out = read_csv(file_name) %>% 
    mutate(run_id = i)
})




freq_predictive_entropy_dirs = grep("predictive_entropy", out_dirs, ignore.case = T, value = T) %>% 
  grep("freq", ., ignore.case = T, value = T, invert = F)

freq_predictive_entropy_accuracies = map_df(seq_along(freq_predictive_entropy_dirs), function(i){
  file_name = grep("accuracies", list.files(freq_predictive_entropy_dirs[i], full.names = T), value = T)
  out = read_csv(file_name) %>% 
    mutate(run_id = i)
})



freq_var_ratios_dirs = grep("var_ratios", out_dirs, ignore.case = T, value = T) %>% 
  grep("freq", ., ignore.case = T, value = T, invert = F)

freq_var_ratios_accuracies = map_df(seq_along(freq_var_ratios_dirs), function(i){
  file_name = grep("accuracies", list.files(freq_var_ratios_dirs[i], full.names = T), value = T)
  out = read_csv(file_name) %>% 
    mutate(run_id = i)
})




accuracies_all <- random_accuracies %>%
  bind_rows(bald_accuracies) %>%
  bind_rows(predictive_entropy_accuracies) %>%
  bind_rows(var_ratios_accuracies) %>%
  bind_rows(freq_predictive_entropy_accuracies) %>%
  bind_rows(freq_var_ratios_accuracies) %>%
  mutate(num_images = 20 + (iter-1)*10)


saveRDS(accuracies_all, "../out/MNIST/accuracies_all.rds")


accuracies_avg = accuracies_all %>% 
  group_by(acq_fun, iter, num_images) %>% 
  summarize(accuracy = mean(accuracy, na.rm = T))



accuracies_avg %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_point(size = 0.3) +
  geom_line(size = 0.2)


accuracies_avg %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_line()


accuracies_avg %>% 
  filter(iter < 30) %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_line() 




