
#################################
# MNIST
#################################

mnist_out_folder = "../out/MNIST/"
dir.create(mnist_out_folder)
mnist_log_filename = paste0(mnist_out_folder, "output_", as.integer(Sys.time()), ".log")
file.create(mnist_log_filename)
con <- file(mnist_log_filename)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

source("MNIST_active_learning.R", echo = F, max.deparse.length = 10000)

# Restore output to console
sink() 
sink(type="message")

rm(list = ls())
gc()

#################################
# CIFAR10
#################################

cifar10_out_folder = "../out/CIFAR10/"
dir.create(cifar10_out_folder)
cifar10_log_filename = paste0(cifar10_out_folder, "output_", as.integer(Sys.time()), ".log")
file.create(cifar10_log_filename)
con <- file(cifar10_log_filename)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

source("CIFAR10_active_learning.R", echo = F, max.deparse.length = 10000)

# Restore output to console
sink() 
sink(type="message")

rm(list = ls())
gc()

#################################
# Cats and dogs
#################################

cats_dogs_out_folder = "../out/cats_dogs/"
dir.create(cats_dogs_out_folder)
cats_dogs_log_filename = paste0(cats_dogs_out_folder, "output_", as.integer(Sys.time()), ".log")

cats_dogs_log_filename = paste0("../out/cats_dogs/output_", as.integer(Sys.time()), ".log")
file.create(cats_dogs_log_filename)
con <- file(cats_dogs_log_filename)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

source("cats_dogs_active_learning.R", echo = F, max.deparse.length = 10000)

# Restore output to console
sink() 
sink(type="message")

rm(list = ls())
gc()

