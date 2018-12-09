library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ggplot2)

# reddish purple, sky blue, bluish green, blue, black, vermilion, gray, yellow, orange
cbPalette <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#000000", "#D55E00", "#999999", "#F0E442")

names_colors = tibble(
  names = c("BALD", "Frequentist pred. ent.", "Frequentist var. ratios",
            "Predictive entropy", "Random", "Variation ratios"),
  acq_fun = c("bald", "freq_predictive_entropy", "freq_var_ratios",
              "predictive_entropy", "random", "var_ratios")) %>% 
  mutate(color = cbPalette[1:nrow(.)])

names_colors_vec = names_colors$color
names(names_colors_vec) = names_colors$names

theme_set(theme_bw())


# My functions ------------------------------------------------------------

bind_accuracies = function(out_subdirs){
  
  random_dirs = grep("random", out_subdirs, ignore.case = T, value = T)
  
  random_accuracies = map_df(seq_along(random_dirs), function(i){
    file_name = grep("accuracies", list.files(random_dirs[i], full.names = T), value = T)
    cat("Doing", file_name, "\n")
    out = read_csv(file_name) %>% 
      mutate(run_id = i)
  })
  
  
  bald_dirs = grep("bald", out_subdirs, ignore.case = T, value = T)
  
  bald_accuracies = map_df(seq_along(bald_dirs), function(i){
    file_name = grep("accuracies", list.files(bald_dirs[i], full.names = T), value = T)
    cat("Doing", file_name, "\n")
    out = read_csv(file_name) %>% 
      mutate(run_id = i)
  })
  
  
  
  predictive_entropy_dirs = grep("predictive_entropy", out_subdirs, ignore.case = T, value = T) %>% 
    grep("freq", ., ignore.case = T, value = T, invert = T)
  
  predictive_entropy_accuracies = map_df(seq_along(predictive_entropy_dirs), function(i){
    file_name = grep("accuracies", list.files(predictive_entropy_dirs[i], full.names = T), value = T)
    cat("Doing", file_name, "\n")
    out = read_csv(file_name) %>% 
      mutate(run_id = i)
  })
  
  
  
  var_ratios_dirs = grep("var_ratios", out_subdirs, ignore.case = T, value = T) %>% 
    grep("freq", ., ignore.case = T, value = T, invert = T)
  
  var_ratios_accuracies = map_df(seq_along(var_ratios_dirs), function(i){
    file_name = grep("accuracies", list.files(var_ratios_dirs[i], full.names = T), value = T)
    cat("Doing", file_name, "\n")
    out = read_csv(file_name) %>% 
      mutate(run_id = i)
  })
  
  
  
  freq_predictive_entropy_dirs = grep("predictive_entropy", out_subdirs, ignore.case = T, value = T) %>% 
    grep("freq", ., ignore.case = T, value = T, invert = F)
  
  freq_predictive_entropy_accuracies = map_df(seq_along(freq_predictive_entropy_dirs), function(i){
    file_name = grep("accuracies", list.files(freq_predictive_entropy_dirs[i], full.names = T), value = T)
    cat("Doing", file_name, "\n")
    out = read_csv(file_name) %>% 
      mutate(run_id = i)
  })
  
  
  
  freq_var_ratios_dirs = grep("var_ratios", out_subdirs, ignore.case = T, value = T) %>% 
    grep("freq", ., ignore.case = T, value = T, invert = F)
  
  freq_var_ratios_accuracies = map_df(seq_along(freq_var_ratios_dirs), function(i){
    file_name = grep("accuracies", list.files(freq_var_ratios_dirs[i], full.names = T), value = T)
    cat("Doing", file_name, "\n")
    out = read_csv(file_name) %>% 
      mutate(run_id = i)
  })
  
  # Create dataframe with all accuracies 
  
  cat("Binding all...")
  
  accuracies_all <- random_accuracies %>%
    bind_rows(bald_accuracies) %>%
    bind_rows(predictive_entropy_accuracies) %>%
    bind_rows(var_ratios_accuracies) %>%
    bind_rows(freq_predictive_entropy_accuracies) %>%
    bind_rows(freq_var_ratios_accuracies)
  
  cat("success.\n\n")
  
  return(accuracies_all)
}


# Read data and create dataframe with all accuracies ----------------------

out_dir = "../out/cats_dogs/"
out_subdirs = list.dirs(out_dir)
accuracies_filename = paste0(out_dir, "accuracies_all.rds")

if(file.exists(accuracies_filename)){
  accuracies_all = readRDS(accuracies_filename)
} else{
  accuracies_all <- bind_accuracies(out_subdirs) %>% 
    # Modificar esto
    mutate(num_images = 100 + (iter-1)*50)
  
  saveRDS(accuracies_all, accuracies_filename)
}


# Transform and plot data -------------------------------------------------

accuracies_avg = accuracies_all %>% 
  group_by(acq_fun, iter, num_images) %>% 
  summarize(accuracy = mean(accuracy, na.rm = T))

max_num_images = max(accuracies_avg$num_images)

accuracies_avg %>% 
  left_join(names_colors) %>% 
  ggplot(aes(num_images, accuracy, color = names)) +
  geom_point(size = 0.3) +
  geom_line(size = 0.4) +
  scale_y_continuous(breaks = seq(0.6, 1, by = 0.05)) +
  scale_x_continuous(breaks = seq(0, max_num_images, by = 50)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "cats_dogs dataset", 
       x = "Number of images", 
       y = "Accuracy on test set", 
       color = "Acquisition\nfunction") +
  scale_color_manual(values = names_colors_vec)


accuracies_avg %>% 
  left_join(names_colors) %>% 
  filter(acq_fun %in% c("freq_predictive_entropy", "predictive_entropy")) %>% 
  ggplot(aes(num_images, accuracy, color = names)) +
  geom_point(size = 0.3) +
  geom_line(size = 0.4) +
  scale_y_continuous(breaks = seq(0.6, 1, by = 0.05)) +
  scale_x_continuous(breaks = seq(0, 1200, by = 20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "cats_dogs dataset", 
       x = "Number of images", 
       y = "Accuracy on test set", 
       color = "Acquisition\nfunction") +
  scale_color_manual(values = names_colors_vec)


accuracies_avg %>% 
  left_join(names_colors) %>% 
  filter(acq_fun %in% c("freq_var_ratios", "var_ratios")) %>% 
  ggplot(aes(num_images, accuracy, color = names)) +
  geom_point(size = 0.3) +
  geom_line(size = 0.4) +
  scale_y_continuous(breaks = seq(0.6, 1, by = 0.05)) +
  scale_x_continuous(breaks = seq(0, 1200, by = 20)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "cats_dogs dataset", 
       x = "Number of images", 
       y = "Accuracy on test set", 
       color = "Acquisition\nfunction") +
  scale_color_manual(values = names_colors_vec)













# accuracies_all %>% 
#   # filter(acq_fun %in% c("bald", 
#   #                       "predictive_entropy", 
#   #                       "var_ratios", 
#   #                       "random")) %>% 
#   ggplot(aes(num_images, accuracy, color = acq_fun)) +
#   geom_point(size = 0.4) +
#   geom_line(size = 0.6) +
#   #scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
#   #scale_y_continuous(breaks = seq(0.6, 1, by = 0.02)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         legend.position="bottom")

# 
# accuracies_all %>% 
#   filter(acq_fun %in% c("bald", 
#                         "predictive_entropy", 
#                         "var_ratios", "random")) %>% 
#   filter(accuracy > 0.65) %>% 
#   ggplot(aes(num_images, accuracy, color = acq_fun)) +
#   geom_point(size = 0.4) +
#   geom_line(size = 0.7) +
#   scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
#   scale_y_continuous(breaks = seq(0.6, 1, by = 0.02)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         legend.position="bottom")
# 
# 
# 
# accuracies_all %>% 
#   mutate(accuracy = 100*accuracy) %>% 
#   filter(acq_fun %in% c("bald", 
#                         "predictive_entropy", 
#                         "var_ratios", "random")) %>% 
#   filter(accuracy >= 80) %>% 
#   ggplot(aes(num_images, accuracy, color = acq_fun)) +
#   geom_point(size = 0.4) +
#   geom_line(size = 0.7) +
#   scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
#   scale_y_continuous(breaks = seq(80, 100, by = 2)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         legend.position="bottom")
# 
# 
# 
# accuracies_all %>% 
#   filter(acq_fun %in% c("predictive_entropy", "freq_predictive_entropy")) %>% 
#   ggplot(aes(num_images, accuracy, color = acq_fun)) +
#   geom_point(size = 0.4) +
#   geom_line(size = 0.6) +
#   scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
#   scale_y_continuous(breaks = seq(0.6, 1, by = 0.02)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         legend.position="bottom")
# 
# 
# accuracies_all %>% 
#   filter(acq_fun %in% c("predictive_entropy", "freq_predictive_entropy")) %>% 
#   ggplot(aes(num_images, accuracy, color = acq_fun)) +
#   geom_point(size = 0.4) +
#   geom_line(size = 0.6) +
#   scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
#   scale_y_continuous(breaks = seq(0.54, 1, by = 0.02)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         legend.position = c(0.95, 0.01), 
#         legend.justification = c(1, 0))
# 
# 
# accuracies_all %>% 
#   filter(acq_fun %in% c("var_ratios", "freq_var_ratios")) %>% 
#   ggplot(aes(num_images, accuracy, color = acq_fun)) +
#   geom_point(size = 0.4) +
#   geom_line(size = 0.6) +
#   scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
#   scale_y_continuous(breaks = seq(0.6, 1, by = 0.02)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         legend.position="bottom")
# 
# 
# 
# accuracies_all %>% 
#   filter(acq_fun %in% c("var_ratios", "freq_var_ratios")) %>% 
#   ggplot(aes(num_images, accuracy, color = acq_fun)) +
#   geom_point(size = 0.4) +
#   geom_line(size = 0.6) +
#   scale_x_continuous(breaks = seq(20, 1000, by = 20)) +
#   scale_y_continuous(breaks = seq(0.54, 1, by = 0.02)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1),
#         legend.position = c(0.95, 0.01), 
#         legend.justification = c(1, 0))



