library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(ggplot2)

train_folder = "../out/cats_dogs/"
if(!dir.exists(train_folder)) dir.create(train_folder)

train_rds_filename = paste0(train_folder, "dat_train_64x64.rds")
train_data_url = "https://www.dropbox.com/s/9t7p4ng5nu8ogc2/dat_train_64x64.rds?dl=1"

if(file.exists(train_rds_filename)){
  message("Reading RDS file.\n\n")
  cats_dogs <- readRDS(train_rds_filename)
} else{
  message("Data file not found. Going to download.\n\n")
  download_res = try(download.file(train_data_url, train_rds_filename))
  if(class(download_res) != "try-error") {
    message("Download successful.")
    cats_dogs <- readRDS(train_rds_filename)
  } else{
    stop("Download failed!!!")
  }
}




plot_image <- function(img){
  
  for(i in 1:3){
    img[,,i] = t(img[,,i])
  }
  
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



plot_image_ix <- function(ix, cats_dogs){
  out = plot_image(cats_dogs$x[ix,,,])
  return(out)
}


plot_images = function(imgs, ncol = 3){
  plots = lapply(imgs, function(i) plot_image_ix(i, cats_dogs))
  out = do.call("grid.arrange", c(plots, ncol = ncol))
  return(out)
}

# plot_image(mnist$train$x[1,,] )

n_width = 10
n_height = 6

set.seed(201812)
imgs_idx = c(sample(which(cats_dogs$y == "cat"), n_width*n_height/2), sample(which(cats_dogs$y == "dog"), n_width*n_height/2))
# imgs_idx = sample(1:nrow(cats_dogs$x), n_width*n_height)

examples = plot_images(imgs_idx, ncol = n_width)

out_dir = "../out/cats_dogs/plots/"
if(!dir.exists(out_dir)) dir.create(out_dir)

ggsave(paste0(out_dir, "plots/cats_dogs_data_examples.png"), examples, dpi = 300, height = n_height, width = n_width, units = "cm")




