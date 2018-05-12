library(tidyverse)

index = read_delim("../data/earthquake/index_2.psv", delim = "|")

index %>% 
  group_by(town) %>% 
  tally()

index %>% 
  group_by(label) %>% 
  tally()

index %>% 
  group_by(town, label) %>% 
  tally()