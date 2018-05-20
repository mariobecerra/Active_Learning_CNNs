library(tidyverse)

theme_set(theme_bw())

accuracies_all <- readRDS("../out/MNIST/accuracies_all.rds")


accuracies_all %>% 
  filter(acq_fun %in% c("bald", 
                        "predictive_entropy", 
                        "var_ratios", "random")) %>% 
  filter(accuracy > 0.65) %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_point(size = 0.3) +
  geom_line(size = 0.3) +
  scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
  scale_y_continuous(breaks = seq(0.6, 1, by = 0.02)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom")


accuracies_all %>% 
  filter(acq_fun %in% c("bald", 
                        "predictive_entropy", 
                        "var_ratios", "random")) %>% 
  filter(accuracy > 0.65) %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_point(size = 0.4) +
  geom_line(size = 0.7) +
  scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
  scale_y_continuous(breaks = seq(0.6, 1, by = 0.02)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom")



accuracies_all %>% 
  mutate(accuracy = 100*accuracy) %>% 
  filter(acq_fun %in% c("bald", 
                        "predictive_entropy", 
                        "var_ratios", "random")) %>% 
  filter(accuracy >= 80) %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_point(size = 0.4) +
  geom_line(size = 0.7) +
  scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
  scale_y_continuous(breaks = seq(80, 100, by = 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom")



accuracies_all %>% 
  filter(acq_fun %in% c("predictive_entropy", "freq_predictive_entropy")) %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_point(size = 0.4) +
  geom_line(size = 0.6) +
  scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
  scale_y_continuous(breaks = seq(0.6, 1, by = 0.02)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom")


accuracies_all %>% 
  filter(acq_fun %in% c("predictive_entropy", "freq_predictive_entropy")) %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_point(size = 0.4) +
  geom_line(size = 0.6) +
  scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
  scale_y_continuous(breaks = seq(0.54, 1, by = 0.02)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = c(0.95, 0.01), 
        legend.justification = c(1, 0))


accuracies_all %>% 
  filter(acq_fun %in% c("var_ratios", "freq_var_ratios")) %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_point(size = 0.4) +
  geom_line(size = 0.6) +
  scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
  scale_y_continuous(breaks = seq(0.6, 1, by = 0.02)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom")



accuracies_all %>% 
  filter(acq_fun %in% c("var_ratios", "freq_var_ratios")) %>% 
  ggplot(aes(num_images, accuracy, color = acq_fun)) +
  geom_point(size = 0.4) +
  geom_line(size = 0.6) +
  scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
  scale_y_continuous(breaks = seq(0.54, 1, by = 0.02)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = c(0.95, 0.01), 
        legend.justification = c(1, 0))



