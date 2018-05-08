library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

theme_set(theme_bw())

accuracies_all <- read_csv("../out/MNIST/random_acq//MNIST_accuracies_so_far_random.csv") %>% 
  bind_rows(
    read_csv("../out/MNIST/var_ratios/MNIST_accuracies_so_far_var_ratios.csv")
  ) %>% 
  bind_rows(
    read_csv("../out/MNIST/bald/MNIST_accuracies_so_far_bald.csv")
  ) %>% 
  bind_rows(
    read_csv("../out/MNIST/freq_var_ratios/MNIST_accuracies_so_far_freq_var_ratios.csv")
  ) %>% 
  bind_rows(
    read_csv("../out/MNIST/predictive_entropy/MNIST_accuracies_so_far_predictive_entropy.csv")
  ) %>%
  mutate(num_images = 20 + (iter-1)*10)

saveRDS(accuracies, "../out/MNIST/accuracies_all.rds")
  
accuracies_all %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_point() +
  geom_line()


accuracies_all %>% 
  ggplot(aes(iter, accuracy, color = acq_fun)) +
  #geom_point() +
  geom_line()

accuracies_all %>% 
  filter(!is.na(accuracy)) %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_line() 

accuracies_all %>% 
  filter(iter < 30) %>% 
  ggplot(aes(iter, accuracy, color = acq_fun)) +
  geom_line() 




