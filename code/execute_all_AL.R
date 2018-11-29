
#################################
# MNIST
#################################

mnist_log_filename = paste0("../out/MNIST/output_", as.integer(Sys.time()), ".log")
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

cifar10_log_filename = paste0("../out/CIFAR10/output_", as.integer(Sys.time()), ".log")
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


cats_dogs_log_filename = paste0("../out/cats_dogs/output_", as.integer(Sys.time()), ".log")
con <- file(cats_dogs_log_filename)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

source("cats_dogs_active_learning.R", echo = F, max.deparse.length = 10000)

# Restore output to console
sink() 
sink(type="message")

rm(list = ls())
gc()

