library(dplyr)
library(tidyr)
library(readr)

accuracies_all <- read_csv("../out/MNIST/random_acq//MNIST_accuracies_so_far_random.csv") %>% 
  bind_rows(
    read_csv("../out/MNIST/var_ratios/MNIST_accuracies_so_far_var_ratios.csv")
  ) %>% 
  mutate(num_images = 20 + iter*10)

saveRDS(accuracies, "../out/MNIST/accuracies_all.rds")
  
accuracies_all %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_point() +
  geom_line()


accuracies_all %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_line() 





