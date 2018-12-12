library(dplyr)
library(ggplot2)


## For reference
palette_original_order = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

palette_names = tibble(
  codes = palette_original_order,
  color = c("black", "orange", "sky blue", "bluish green", "yellow", "blue", "vermilion", "reddish purple", "grey")
) %>% 
  arrange(color)

# Plot palette
palette_plot = palette_names %>% 
  mutate(color2 = paste(color, codes, sep = "\n"),
         y = 1) %>% 
  ggplot() +
  geom_bar(aes(x = color2, y = y, fill = color), stat = 'identity') +
  scale_fill_manual(values = palette_names$codes)


# Palette for acquisition functions
cbPalette <- c("#D55E00", "#E69F00", "#CC79A7", "#009E73", "#000000","#56B4E9", "#0072B2", "#999999", "#F0E442")

names_colors = tibble(
  names = c("BALD", "Frequentist predictive entropy", "Frequentist variation ratios",
            "Predictive entropy", "Random", "Variation ratios"),
  acq_fun = c("bald", "freq_predictive_entropy", "freq_var_ratios",
              "predictive_entropy", "random", "var_ratios")) %>% 
  mutate(color = cbPalette[1:nrow(.)])

names_colors_vec = names_colors$color
names(names_colors_vec) = names_colors$names


# My functions ------------------------------------------------------------


my_theme = function(...) {
  theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.justification = c(1.05, -0.05), 
          #legend.background = element_rect(fill = NA),
          legend.background = element_rect(colour = "black",
                                           size = 0.1),
          legend.position = c(1, 0),
          panel.border = element_blank()
    )
}


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