library(tidyverse)
library(imager)

# https://stackoverflow.com/questions/35786744/resizing-image-in-r

# im <- load.image("~/Desktop/cats_dogs/")
# plot(im)
# # ras <- as.raster(im)
# # plot(ras)
# 
# thmb <- resize(im, 64, 64)
# plot(thmb)

out_width = 64
out_height = 64

index = list.files("../data/cats_dogs/train/")
labels = substr(index, 1, 3)
n_files = length(index)

## Create list of arrays with the appropiate size.
## This allows us to see if it fits in memory before starting reading images.

dat_train = lapply(seq_along(index), function(i){
  label = labels[i]
  image = array(data = rep(0, out_width*out_height*3), dim = c(out_width, out_height, 3))
  return(
    list(
      label = label,
      image = image
    )
  )
})

for(i in seq_along(index)){
  if(i %% 100 == 0) cat("\tReading ", index[i], " (", i, " of ", n_files, ")\n", sep = "")
  #im <- load.image(paste0("../data/cats_dogs/train/", index[i]))
  image <- drop(as.array(resize(load.image(paste0("../data/cats_dogs/train/", index[i])), 
                                out_width, 
                                out_height)))
  dat_train[[i]]$image = image
}

dir.create("../out/cats_vs_dogs/")
saveRDS(dat_train, 
        paste0("../out/cats_vs_dogs/dat_train_", out_width, "x", out_height, ".rds"))

paste0("../out/cats_vs_dogs/dat_train_", out_width, "x", out_height, ".rds")

# dat_train = lapply(seq_along(index), function(i){
#   label = labels[i]
#   # image = array(data = rep(0, out_width*out_height*3), dim = c(out_width, out_height, 1, 3))
# 
#   if(i %% 100 == 0) cat("\tReading ", index[i], " (", i, " of ", n_files, ")\n", sep = "")
#   im <- load.image(paste0("../data/cats_dogs/train/", index[i]))
#   image <- drop(as.array(resize(im, out_width, out_height)))
#   # plot(imager::as.cimg(image))
# 
#   return(
#     list(
#       label = label,
#       image = image
#     )
#   )
# })


