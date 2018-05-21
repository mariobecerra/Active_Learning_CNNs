library(imager)
library(tidyverse)

index = read_delim("../data/earthquake/index_3.psv", delim = "|")

towns = index$town %>% unique()
labels = index$label %>% unique()

orig_folder = "../out/earthquake_folders_data/"
dest_folder = "../out/earthquake_folders_data_resized/"
dir.create(dest_folder)
for(i in 1:length(towns)){
  dir.create(paste0(dest_folder, towns[i]))
}

for(i in 1:length(towns)){
  dir.create(paste0(dest_folder, towns[i], "/", labels[1]))
  dir.create(paste0(dest_folder, towns[i], "/", labels[2]))
}


#### resize images

for(i in seq_along(towns)){
  cat("Town:", towns[i], "\n")
  for(j in seq_along(labels)){
    cat("\tLabel:", labels[j], "\n")
    org_folder = paste0(orig_folder, towns[i], "/", labels[j], "/")
    image_list = list.files(org_folder)
    n_files = length(image_list)
    count = 0
    for(img_file in image_list){
      count = count + 1
      cat("\t\tReading", img_file, "(", count, "of", n_files, ")...")
      im <- load.image(paste0(org_folder, img_file))
      cat("read.\n")
      cat("\t\t\tResizing...")
      im_resized <- resize(im, 224, 224)
      cat("resized.\n")
      cat("\t\t\tSaving...")
      save.image(im_resized, paste0(dest_folder, towns[i], "/", labels[j], "/", img_file))
      cat("saved.\n")
    }
  }
}
