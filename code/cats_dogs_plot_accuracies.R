library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ggplot2)

source("plot_accuracies_utils.R")

# Read data and create dataframe with all accuracies ----------------------

out_dir = "../out/cats_dogs/"
out_subdirs = list.dirs(out_dir)
accuracies_filename = paste0(out_dir, "accuracies_all.rds")

if(file.exists(accuracies_filename)){
  message("Reading RDS file.")
  accuracies_all = readRDS(accuracies_filename)
} else{
  message("Reading folders.")
  accuracies_all <- bind_accuracies(out_subdirs) %>% 
    # Modificar esto
    mutate(num_images = 100 + (iter-1)*50)
  
  saveRDS(accuracies_all, accuracies_filename)
}


# Transform and plot data -------------------------------------------------


plot_out_dir = paste0(out_dir, "plots/")
dir.create(plot_out_dir)

# Plot file dimensions
# out_width = 20
# out_height = 10
out_units = "cm"
out_dpi = 300

# Plot, axis and legend names
plot_title = NULL
x_label = NULL 
y_label = "Accuracy on test set"
legend_name = "Acquisition function"

# Dataframe for plotting
accuracies_avg = accuracies_all %>% 
  mutate(accuracy = accuracy*100) %>% 
  group_by(acq_fun, iter, num_images) %>% 
  summarize(
    upper = quantile(accuracy, 0.95, na.rm = T),
    lower = quantile(accuracy, 0.05, na.rm = T),
    accuracy = mean(accuracy, na.rm = T)
  )

max_num_images = max(accuracies_avg$num_images)


## Bayesian acquisition functions

(accuracies_avg %>% 
    filter(acq_fun %in% c("var_ratios", "predictive_entropy", "bald", "random")) %>% 
    left_join(names_colors) %>% 
    ggplot(aes(num_images, accuracy, color = names)) +
    geom_point(size = 0.3) +
    geom_line(size = 0.4) +
    scale_x_continuous(breaks = seq(0, max_num_images, by = 100)) +
    my_theme() +
    labs(
      title = plot_title,
      x = x_label, 
      y = y_label, 
      color = legend_name) +
    scale_color_manual(values = names_colors_vec)
)  %>% 
  ggsave(filename = paste0(plot_out_dir, "cats_dogs_accuracies_bayesian.png"), 
         .,
         width = 20,
         height = 10,
         units = out_units,
         dpi = out_dpi)


## Predictive entropy

(accuracies_avg %>% 
    left_join(names_colors) %>% 
    filter(acq_fun %in% c("freq_predictive_entropy", "predictive_entropy")) %>% 
    ggplot(aes(num_images, accuracy)) +
    geom_point(size = 0.6, aes(color = names)) +
    geom_line(size = 0.5, aes(color = names)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = names), alpha = 0.3, color = NA) +
    scale_x_continuous(breaks = seq(0, max_num_images, by = 100)) +
    my_theme() +
    labs(
      title = plot_title,
      x = x_label, 
      y = y_label, 
      color = legend_name,
      fill = legend_name) +
    scale_color_manual(values = names_colors_vec) +
    scale_fill_manual(values = names_colors_vec)
) %>% 
  ggsave(filename = paste0(plot_out_dir, "cats_dogs_accuracies_pred_ent_ribbon.png"), 
         .,
         width = 13,
         height = 11,
         units = out_units,
         dpi = out_dpi)




## Variation ratios

(accuracies_avg %>% 
    left_join(names_colors) %>% 
    filter(acq_fun %in% c("freq_var_ratios", "var_ratios")) %>% 
    ggplot(aes(num_images, accuracy)) +
    geom_point(size = 0.6, aes(color = names)) +
    geom_line(size = 0.5, aes(color = names)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = names), alpha = 0.3, color = NA) +
    scale_x_continuous(breaks = seq(0, max_num_images, by = 100)) +
    my_theme() +
    labs(
      title = plot_title,
      x = x_label, 
      y = y_label, 
      color = legend_name,
      fill = legend_name) +
    scale_color_manual(values = names_colors_vec) +
    scale_fill_manual(values = names_colors_vec)
) %>% 
  ggsave(filename = paste0(plot_out_dir, "cats_dogs_accuracies_var_ratios_ribbon.png"), 
         .,
         width = 13,
         height = 11,
         units = out_units,
         dpi = out_dpi)
