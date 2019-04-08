library(keras)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ggplot2)

# Saves some of the pictures that the model considered had high uncertainty.

mnist = dataset_mnist()

aaa = readRDS("../out/MNIST/var_ratios_2018-11-15_19.19.19/train_pool_ix_var_ratios.rds")


plot_image <- function(img){
  out = img %>% 
    as_tibble() %>% 
    mutate(y = seq(nrow(.), 1, by = -1)) %>% 
    gather(x, value, -y) %>% 
    mutate(x = as.integer(gsub("V", "", x))) %>% 
    ggplot() +
    geom_raster(aes(x, y, fill = value)) +
    coord_equal() +
    scale_fill_continuous(low = "white", high = "black") +
    theme_void() +
    theme(legend.position = "none")
  return(out)
}

plot_images = function(imgs, ncol = 3){
  plots = lapply(imgs, plot_image)
  out = do.call("grid.arrange", c(plots, ncol = ncol))
  return(out)
}

# plot_image(mnist$train$x[1,,] )

n_images_side = 10

imgs = as.vector(sapply(46:55, function(i) aaa[[i]]$id_highest_uncertainty))

examples = plot_images(lapply(imgs, function(i) mnist$train$x[i,,]), ncol = n_images_side)


out_dir = "../out/MNIST/plots/"
if(!dir.exists(out_dir)) dir.create(out_dir)
ggsave(paste0(out_dir, "MNIST_high_uncertainty_examples.png"), examples, dpi = 300, height = 5, width = 7, units = "cm")

