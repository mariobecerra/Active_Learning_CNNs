library(dplyr)
library(tidyr)
library(readr)

accuracies_all <- read_csv("../out/MNIST/bald/MNIST_bald_accuracies_so_far.csv") %>% 
  mutate(iter = 1:nrow(.)) %>% 
  bind_rows(
    read_csv("../out/MNIST/predictive_entropy/MNIST_predictive_entropy_accuracies_so_far.csv") %>% 
      mutate(iter = 1:nrow(.))
  ) %>% 
  bind_rows(
    read_csv("../out/MNIST/random_acq/MNIST_random_accuracies_so_far.csv") %>% 
      mutate(iter = 1:nrow(.))
  ) %>% 
  bind_rows(
    read_csv("../out/MNIST/var_ratios/MNIST_var_ratios_accuracies_so_far.csv") %>% 
      mutate(iter = 1:nrow(.))
  ) %>% 
  mutate(num_images = 20 + (iter-1)*10)

saveRDS(accuracies, "../out/MNIST/accuracies_all.rds")
  
accuracies_all %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_point() +
  geom_line()


accuracies_all %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_line() 





