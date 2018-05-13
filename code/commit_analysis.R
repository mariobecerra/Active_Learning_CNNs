# https://www.r-bloggers.com/guide-to-tidy-git-analysis/

# Parts 1 and 2
library(tidyverse)
library(glue)
library(stringr)
library(forcats)
library(lubridate)

# Part 3
library(tidygraph)
# library(ggraph)
library(tidytext)


clone_dir <- file.path("../")

system(glue('git -C ../ log -3'))

log_format_options <- c(datetime = "cd", commit = "h", parents = "p", author = "an", subject = "s")
option_delim <- "\t"
log_format <- glue("%{log_format_options}") %>% collapse(option_delim)
log_options <- glue('--pretty=format:"{log_format}" --date=format:"%Y-%m-%d %H:%M:%S"')
log_cmd <- glue('git -C {clone_dir} log {log_options}')
log_cmd

system(glue('{log_cmd} -3'))

history_logs <- system(log_cmd, intern = TRUE) %>% 
  str_split_fixed(option_delim, length(log_format_options)) %>% 
  as_tibble() %>% 
  setNames(names(log_format_options)) %>% 
  mutate(parents = str_split(parents, " ")) %>% 
  mutate(branch = NA_integer_) %>% 
  mutate(datetime = lubridate::ymd_hms(datetime),
         date = lubridate::date(datetime),
         Day = lubridate::wday(datetime, label = T))


# # assign branch numbers to commits
# # Create a boolean vector to represent free columns (1000 should be plenty!)
# free_col <- rep(TRUE, 1000)
# 
# for (i in seq_len(nrow(history_logs) - 1)) { # - 1 to ignore root
#   # Check current branch col and assign open col if NA
#   branch <- history_logs$branch[i]
#   
#   if (is.na(branch)) {
#     branch <- which.max(free_col)
#     free_col[branch] <- FALSE
#     history_logs$branch[i] <- branch
#   }
#   
#   # Go through parents
#   parents <- history_logs$parents[[i]]
#   
#   for (p in parents) {
#     parent_col <- history_logs$branch[history_logs$commit == p]
#     
#     # If col is missing, assign it to same branch (if first parent) or new
#     # branch (if other)
#     if (is.na(parent_col)) {
#       parent_col <- if_else(p == parents[1], branch, which.max(free_col))
#       
#       # If NOT missing this means a split has occurred. Assign parent the lowest
#       # and re-open both cols (parent closed at the end)
#     } else {
#       free_col[c(branch, parent_col)] <- TRUE
#       parent_col <- min(branch, parent_col)
#       
#     }
#     
#     # Close parent col and assign
#     free_col[parent_col] <- FALSE
#     history_logs$branch[history_logs$commit == p] <- parent_col
#   }
# }


history_logs

history_logs %>% 
  count(author, sort = TRUE)

history_logs %>% 
  group_by(Day) %>% 
  tally() %>% 
  ggplot() +
  geom_bar(aes(Day, n), stat = "identity")

history_logs %>% 
  group_by(date) %>% 
  tally() %>% 
  ggplot(aes(date, n)) +
  geom_point() +
  geom_line()


history_logs %>% 
  mutate(time = hour(datetime) + 1) %>% 
  mutate(effective_date = lubridate::as_date(ifelse(time < 4, date - days(1), date))) %>% 
  group_by(effective_date) %>% 
  summarize(min_hour = min(datetime),
            max_hour = max(datetime)) %>% 
  mutate(minutes = difftime(max_hour, min_hour, units = "mins"),
         Day = lubridate::wday(effective_date, label = T)) %>% 
  group_by(Day) %>% 
  summarize(minutes = sum(minutes)) %>% 
  ggplot() +
  geom_bar(aes(Day, minutes), stat = 'identity')









