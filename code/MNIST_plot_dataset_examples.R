library(keras)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ggplot2)

mnist = dataset_mnist()

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

n_images_side = 15
set.seed(201812)
imgs = lapply(sample(1:60000, n_images_side^2), function(i) mnist$train$x[i,,])

examples = plot_images(imgs, ncol = n_images_side)


out_dir = "../out/MNIST/plots/"
if(!dir.exists(out_dir)) dir.create(out_dir)
ggsave(paste0(out_dir, "MNIST_data_examples.png"), examples, dpi = 300, height = 5, width = 7, units = "cm")

