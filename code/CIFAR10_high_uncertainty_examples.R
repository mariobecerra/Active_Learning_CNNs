library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ggplot2)


if(file.exists("../out/cifar10.rds")){
  cifar10 <- readRDS("../out/cifar10.rds")
} else{
  cifar10 <- dataset_cifar10()  
  saveRDS(cifar10, "../out/cifar10.rds")
}



plot_image <- function(img){
  
  # for(i in 1:3){
  #   img[,,i] = t(img[,,i])
  # }
  
  max_dat = max(unlist(img))
  if(max_dat != 1) {
    img = img/max_dat
  } 
  
  col <- rgb(img[,,1], 
             img[,,2],
             img[,,3])
  
  
  dim(col) <- dim(img)[1:2]
  
  # label_string = as.character(catalog[label+1,2])
  # 
  # print(label_string)
  out = grid::rasterGrob(col, interpolate=FALSE)
  return(out)
}


plot_image_ix <- function(ix, cifar10){
  out = plot_image(cifar10$train$x[ix,,,])
  return(out)
}



plot_images = function(imgs, ncol = 3){
  plots = lapply(imgs, function(i) plot_image_ix(i, cifar10))
  out = do.call("grid.arrange", c(plots, ncol = ncol))
  return(out)
}

# plot_image(mnist$train$x[1,,] )

n_width = 20
n_height = 10

aaa = readRDS("../out/CIFAR10/var_ratios_2018-12-11_14.24.10/train_pool_ix_var_ratios.rds")
# imgs_idx = sample(1:nrow(cifar10$train$x), n_width*n_height)
set.seed(2019)
imgs_idx = sample(aaa$iter_020$id_highest_uncertainty, n_width*n_height)

examples = plot_images(imgs_idx, ncol = n_width)

out_dir = "../out/CIFAR10/plots/"
if(!dir.exists(out_dir)) dir.create(out_dir)
ggsave(paste0(out_dir, "CIFAR10_high_uncertainty_examples.png"), examples, dpi = 300, height = n_height, width = n_width, units = "cm")








