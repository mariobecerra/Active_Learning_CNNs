library(tidyverse)

index = read_delim("../data/earthquake/index_3.psv", delim = "|")

# index %>% 
#   group_by(town) %>% 
#   tally()
# 
# index %>% 
#   group_by(label) %>% 
#   tally()
# 
# index %>% 
#   group_by(town, label) %>% 
#   tally()

#### Preprocessing

towns = index$town %>% unique()
labels = index$label %>% unique()

# Create destination folders

dest_folder = "../out/earthquake_folders_data/"
dir.create(dest_folder)
for(i in 1:length(towns)){
  dir.create(paste0(dest_folder, towns[i]))
}

for(i in 1:length(towns)){
  dir.create(paste0(dest_folder, towns[i], "/", labels[1]))
  dir.create(paste0(dest_folder, towns[i], "/", labels[2]))
}


#### Copy files

for(i in 1:nrow(index)){
  origin_file = paste0("../data/earthquake/", index$filename[i])
  cat("Copying file", origin_file, "(", i, "of", nrow(index), ")", "...")
  dest_folder = paste0("../out/earthquake_folders_data/", index$town[i], "/", index$label[i], "/")
  success = file.copy(origin_file, dest_folder)
  if(success) cat("Success\n")
  else cat("File not copied\n")
}


