library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ggplot2)

source("plot_accuracies_utils.R")


# Read data and create dataframe with all accuracies ----------------------

out_dir = "../out/MNIST/"
out_subdirs = list.dirs(out_dir)
accuracies_filename = paste0(out_dir, "accuracies_all.rds")

if(file.exists(accuracies_filename)){
  accuracies_all = readRDS(accuracies_filename)
} else{
  accuracies_all <- bind_accuracies(out_subdirs) %>% 
    mutate(num_images = 20 + (iter-1)*10)
  
  saveRDS(accuracies_all, accuracies_filename)
}


# Transform and plot data -------------------------------------------------

plot_out_dir = paste0(out_dir, "plots/")
dir.create(plot_out_dir)

# Plot file dimensions
out_width = 13
out_height = 11
out_units = "cm"
out_dpi = 300

# Plot, axis and legend names
plot_title = NULL #"MNIST dataset"
x_label = NULL #"Number of images"
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


## Bayesian acquisition functions

(accuracies_avg %>% 
    filter(acq_fun %in% c("var_ratios", "predictive_entropy", "bald", "random")) %>% 
    left_join(names_colors) %>% 
    ggplot(aes(num_images, accuracy, color = names)) +
    geom_point(size = 0.3) +
    geom_line(size = 0.4) +
    scale_y_continuous(breaks = seq(60, 100, by = 2)) +
    scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
    coord_cartesian(ylim = c(80, 100),
                    xlim = c(100, 1000))  +
    my_theme() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
          panel.border = element_blank()) +
    labs(
      title = plot_title,
      x = x_label, 
      y = y_label, 
      color = legend_name) +
    scale_color_manual(values = names_colors_vec)
) %>% 
  ggsave(filename = paste0(plot_out_dir, "MNIST_accuracies_bayesian.png"), 
         .,
         width = out_width,
         height = out_height,
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
    scale_y_continuous(breaks = seq(60, 100, by = 2)) +
    scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
    coord_cartesian(ylim = c(80, 100),
                    xlim = c(100, 1000))  +
    my_theme() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
          panel.border = element_blank()) +
    labs(
      title = plot_title,
      x = x_label, 
      y = y_label, 
      color = legend_name,
      fill = legend_name) +
    scale_color_manual(values = names_colors_vec) +
    scale_fill_manual(values = names_colors_vec)
) %>% 
  ggsave(filename = paste0(plot_out_dir, "MNIST_accuracies_pred_ent_ribbon.png"), 
         .,
         width = out_width,
         height = out_height,
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
    scale_y_continuous(breaks = seq(60, 100, by = 2)) +
    scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
    coord_cartesian(ylim = c(80, 100),
                    xlim = c(100, 1000))  +
    my_theme() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
          panel.border = element_blank()) +
    labs(
      title = plot_title,
      x = x_label, 
      y = y_label, 
      color = legend_name,
      fill = legend_name) +
    scale_color_manual(values = names_colors_vec) +
    scale_fill_manual(values = names_colors_vec)
) %>% 
  ggsave(filename = paste0(plot_out_dir, "MNIST_accuracies_var_ratios_ribbon.png"), 
         .,
         width = out_width,
         height = out_height,
         units = out_units,
         dpi = out_dpi)







# ## All acquisition functions
# 
# (
#   accuracies_avg %>% 
#     left_join(names_colors) %>% 
#     ggplot(aes(num_images, accuracy)) +
#     geom_point(size = 0.3, aes(color = names)) +
#     geom_line(size = 0.4, aes(color = names)) +
#     scale_y_continuous(breaks = seq(0.6, 1, by = 0.05)) +
#     scale_x_continuous(breaks = seq(0, 1200, by = 20)) +
#     my_theme() +
#     labs(title = plot_title, 
#          x = x_label, 
#          y = y_label, 
#          color = legend_name,
#          fill = legend_name) +
#     scale_color_manual(values = names_colors_vec) 
# ) %>% 
#   ggsave(filename = paste0(plot_out_dir, "MNIST_accuracies_all.png"), 
#          .,
#          width = out_width,
#          height = out_height,
#          units = out_units,
#          dpi = out_dpi)
# 
# 
# (
#   accuracies_avg %>% 
#     left_join(names_colors) %>% 
#     ggplot(aes(num_images, accuracy)) +
#     geom_point(size = 0.3, aes(color = names)) +
#     geom_line(size = 0.4, aes(color = names)) +
#     geom_ribbon(aes(ymin = lower, ymax = upper, fill = names), alpha = 0.3, color = NA) +
#     scale_y_continuous(breaks = seq(0.6, 1, by = 0.05)) +
#     scale_x_continuous(breaks = seq(0, 1200, by = 20)) +
#     my_theme() +
#     labs(title = plot_title, 
#          x = x_label, 
#          y = y_label, 
#          color = legend_name,
#          fill = legend_name) +
#     scale_color_manual(values = names_colors_vec) +
#     scale_fill_manual(values = names_colors_vec)
# ) %>% 
#   ggsave(filename = paste0(plot_out_dir, "MNIST_accuracies_all_ribbon.png"), 
#          .,
#          width = out_width,
#          height = out_height,
#          units = out_units,
#          dpi = out_dpi)


# (
#   accuracies_avg %>% 
#     left_join(names_colors) %>% 
#     filter(acq_fun %in% c("freq_predictive_entropy", "predictive_entropy")) %>% 
#     ggplot(aes(num_images, accuracy, color = names)) +
#     geom_point(size = 0.3) +
#     geom_line(size = 0.4) +
#     scale_y_continuous(breaks = seq(0.6, 1, by = 0.05)) +
#     scale_x_continuous(breaks = seq(0, 1200, by = 20)) +
#     my_theme() +
#     labs(title = plot_title, 
#          x = x_label, 
#          y = y_label, 
#          color = legend_name) +
#     scale_color_manual(values = names_colors_vec)
# ) %>% 
#   ggsave(filename = paste0(plot_out_dir, "MNIST_accuracies_pred_ent.png"), 
#          .,
#          width = out_width,
#          height = out_height,
#          units = out_units,
#          dpi = out_dpi)

# 
# (
#   accuracies_avg %>% 
#     left_join(names_colors) %>% 
#     filter(acq_fun %in% c("freq_var_ratios", "var_ratios")) %>% 
#     ggplot(aes(num_images, accuracy, color = names)) +
#     geom_point(size = 0.3) +
#     geom_line(size = 0.4) +
#     scale_y_continuous(breaks = seq(0.6, 1, by = 0.05)) +
#     scale_x_continuous(breaks = seq(0, 1200, by = 20)) +
#     my_theme() +
#     labs(title = plot_title, 
#          x = x_label, 
#          y = y_label, 
#          color = legend_name) +
#     scale_color_manual(values = names_colors_vec)
# ) %>% 
#   ggsave(filename = paste0(plot_out_dir, "MNIST_accuracies_var_ratios.png"), 
#          .,
#          width = out_width,
#          height = out_height,
#          units = out_units,
#          dpi = out_dpi)
# 



# accuracies_avg %>% 
#   ggplot(aes(num_images, accuracy, color = acq_fun)) +
#   geom_point(size = 0.3) +
#   geom_line(size = 0.2)
# 
# 
# accuracies_avg %>% 
#   ggplot(aes(num_images, accuracy, color = acq_fun)) +
#   geom_line()
# 
# 
# accuracies_avg %>% 
#   filter(iter < 30) %>% 
#   ggplot(aes(num_images, accuracy, color = acq_fun)) +
#   geom_line() 


# Old files:

# accuracies_old %>% 
#   filter(acq_fun == "bald") %>% 
#   write_csv(., "../out/MNIST/bald_2018-05/MNIST_accuracies_so_far_bald.csv")
# 
# accuracies_old %>% 
#   filter(acq_fun == "freq_predictive_entropy") %>% 
#   write_csv(., "../out/MNIST/freq_predictive_entropy_2018-05/MNIST_accuracies_so_far_bald.csv")
# 
# accuracies_old %>% 
#   filter(acq_fun == "freq_var_ratios") %>% 
#   write_csv(., "../out/MNIST/freq_var_ratios_2018-05/MNIST_accuracies_so_far_bald.csv")
# 
# accuracies_old %>% 
#   filter(acq_fun == "predictive_entropy") %>% 
#   write_csv(., "../out/MNIST/predictive_entropy_2018-05/MNIST_accuracies_so_far_bald.csv")
# 
# accuracies_old %>% 
#   filter(acq_fun == "var_ratios") %>% 
#   write_csv(., "../out/MNIST/var_ratios_2018-05/MNIST_accuracies_so_far_bald.csv")



