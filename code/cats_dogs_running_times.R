library(dplyr)
library(ggplot2)
library(purrr)
library(lubridate)

out_dir = "../out/cats_dogs/"
out_subdirs = list.dirs(out_dir)


random_dirs = grep("random", out_subdirs, ignore.case = T, value = T)
bald_dirs = grep("bald", out_subdirs, ignore.case = T, value = T)
predictive_entropy_dirs = grep("predictive_entropy", out_subdirs, ignore.case = T, value = T) %>% 
  grep("freq", ., ignore.case = T, value = T, invert = T)
var_ratios_dirs = grep("var_ratios", out_subdirs, ignore.case = T, value = T) %>% 
  grep("freq", ., ignore.case = T, value = T, invert = T)
freq_predictive_entropy_dirs = grep("predictive_entropy", out_subdirs, ignore.case = T, value = T) %>% 
  grep("freq", ., ignore.case = T, value = T, invert = F)
freq_var_ratios_dirs = grep("var_ratios", out_subdirs, ignore.case = T, value = T) %>% 
  grep("freq", ., ignore.case = T, value = T, invert = F)


get_running_times = function(dirs, acq_func){
  out_running_times = map_df(seq_along(dirs), function(i){
    file_name = grep("log_file", list.files(dirs[i], full.names = T), value = T)
    cat("Doing", file_name, "\n")
    if(length(file_name) > 0){
      lines = readLines(file_name)
      times = ymd_hms(lines)
      diff_hours = as.numeric(difftime(times[2], times[1], units = "hours"))
      
    }else {
      diff_hours = NA
    }
    out = tibble(diff_hours = diff_hours) %>% 
      mutate(run_id = i,
             acq_func = acq_func)
    
    return(out)
  })
  return(out_running_times)
}

running_times_all = get_running_times(random_dirs, "random") %>% 
  bind_rows(get_running_times(bald_dirs, "bald")) %>%
  bind_rows(get_running_times(predictive_entropy_dirs, "predictive_entropy")) %>%
  bind_rows(get_running_times(var_ratios_dirs, "var_ratios")) %>%
  bind_rows(get_running_times(freq_predictive_entropy_dirs, "freq_predictive_entropy")) %>%
  bind_rows(get_running_times(freq_var_ratios_dirs, "freq_var_ratios"))


running_times_all %>% 
  ggplot() +
  geom_boxplot(aes(x = acq_func, y = diff_hours))


running_times_all %>% 
  group_by(acq_func) %>% 
  filter(diff_hours == min(diff_hours, na.rm = T)) %>% 
  ggplot() +
  geom_bar(aes(acq_func, diff_hours), stat = 'identity')



