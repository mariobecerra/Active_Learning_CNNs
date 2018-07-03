library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

theme_set(theme_bw())

accuracies_all <- read_csv("../out/MNIST/random_acq/MNIST_accuracies_so_far_random.csv") %>% 
  bind_rows(
    read_csv("../out/MNIST/var_ratios/MNIST_accuracies_so_far_var_ratios.csv")
  ) %>% 
  bind_rows(
    read_csv("../out/MNIST/bald/MNIST_accuracies_so_far_bald.csv")
  ) %>% 
  bind_rows(
    read_csv("../out/MNIST/predictive_entropy/MNIST_accuracies_so_far_predictive_entropy.csv")
  ) %>%
  bind_rows(
    read_csv("../out/MNIST/freq_var_ratios/MNIST_accuracies_so_far_freq_var_ratios.csv")
  ) %>% 
  bind_rows(
    read_csv("../out/MNIST/freq_predictive_entropy/MNIST_accuracies_so_far_freq_predictive_entropy.csv")
  ) %>% 
  bind_rows(
    read_csv("../out/MNIST/predictive_entropy_2018-05-07_no_dropout_test_time_50_MC_samples/MNIST_accuracies_so_far_predictive_entropy.csv") %>% 
      mutate(acq_fun = "pred_ent_no_dropout_test")
  ) %>%
  bind_rows(
    read_csv("../out/MNIST/var_ratios_2018-05-07_no_dropout_test_time_50_MC_samples/MNIST_accuracies_so_far_var_ratios.csv") %>% 
      mutate(acq_fun = "var_rat_no_dropout_test")
  ) %>% 
  bind_rows(
    read_csv("../out/MNIST/bald_2018-05-07_no_dropout_test_time_50_MC_samples/MNIST_accuracies_so_far_bald.csv") %>% 
      mutate(acq_fun = "bald_no_dropout_test")
  ) %>%
  mutate(num_images = 20 + (iter-1)*10)




accuracies_all_2 <- read_csv("../out/MNIST/random_acq//MNIST_accuracies_so_far_random.csv") %>% 
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
  bind_rows(
    read_csv("../out/MNIST/bald_2018-05-07_no_dropout_test_time/MNIST_accuracies_so_far_bald.csv") %>% 
      mutate(acq_fun = "bald_no_dropout_test")
  ) %>%
  bind_rows(
    read_csv("../out/MNIST/predictive_entropy_2018-05-07_no_dropout_test_time/MNIST_accuracies_so_far_predictive_entropy.csv") %>% 
      mutate(acq_fun = "pred_ent_no_dropout_test")
  ) %>%
  bind_rows(
    read_csv("../out/MNIST/var_ratios_2018-05-07_no_dropout_test_time/MNIST_accuracies_so_far_var_ratios.csv") %>% 
      mutate(acq_fun = "var_rat_no_dropout_test")
  ) %>%
  mutate(num_images = 20 + (iter-1)*10)



read_csv("../out/MNIST/predictive_entropy/MNIST_accuracies_so_far_predictive_entropy.csv") %>%
  bind_rows(
    read_csv("../out/MNIST/freq_predictive_entropy/MNIST_accuracies_so_far_freq_predictive_entropy.csv")
  ) %>% 
  filter(accuracy > 0.6) %>%
  ggplot(aes(iter, accuracy, color = acq_fun)) +
  geom_point(size = 0.3) +
  geom_line(size = 0.2)




read_csv("../out/MNIST/var_ratios/MNIST_accuracies_so_far_var_ratios.csv") %>%
  bind_rows(
    read_csv("../out/MNIST/freq_var_ratios/MNIST_accuracies_so_far_freq_var_ratios.csv")
  ) %>% 
  filter(accuracy > 0.6) %>%
  ggplot(aes(iter, accuracy, color = acq_fun)) +
  geom_point(size = 0.3) +
  geom_line(size = 0.2)



read_csv("../out/MNIST/random_acq/MNIST_accuracies_so_far_random.csv") %>% 
  bind_rows(
    read_csv("../out/MNIST/var_ratios/MNIST_accuracies_so_far_var_ratios.csv")
  ) %>% 
  bind_rows(
    read_csv("../out/MNIST/bald/MNIST_accuracies_so_far_bald.csv")
  ) %>% 
  bind_rows(
    read_csv("../out/MNIST/predictive_entropy/MNIST_accuracies_so_far_predictive_entropy.csv")
  ) %>% 
  filter(accuracy > 0.6) %>%
  ggplot(aes(iter, accuracy, color = acq_fun)) +
  geom_point(size = 0.3) +
  geom_line(size = 0.2)




accuracies_all_2 %>% 
  ggplot(aes(iter, accuracy, color = acq_fun)) +
  #geom_point() +
  geom_line()

accuracies_all_2 %>% 
  filter(acq_fun %in% c("bald", "bald_no_dropout_test",
                        "predictive_entropy", "pred_ent_no_dropout_test",
                        "var_ratios", "var_rat_no_dropout_test")) %>% 
  ggplot(aes(iter, accuracy, color = acq_fun)) +
  #geom_point() +
  geom_line()


accuracies_all_2 %>% 
  filter(acq_fun %in% c("bald", "bald_no_dropout_test")) %>% 
  ggplot(aes(iter, accuracy, color = acq_fun)) +
  #geom_point() +
  geom_line()

accuracies_all_2 %>% 
  filter(acq_fun %in% c("predictive_entropy", "pred_ent_no_dropout_test")) %>% 
  ggplot(aes(iter, accuracy, color = acq_fun)) +
  #geom_point() +
  geom_line()

accuracies_all_2 %>% 
  filter(acq_fun %in% c("var_ratios", "var_rat_no_dropout_test")) %>% 
  ggplot(aes(iter, accuracy, color = acq_fun)) +
  #geom_point() +
  geom_line()






saveRDS(accuracies_all, "../out/MNIST/accuracies_all.rds")
  
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




