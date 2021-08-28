
library(tidyverse)

# Set which file to start at 
start_at = 1

files <- list.files()

files_df <- data.frame(file_name = files) %>% 
  filter(str_detect(file_name, ".Rmd")) %>% 
  filter(file_name != "data-reshaping.Rmd")

to_run <- files_df$file_name

run_files <- to_run[start_at:length(to_run)]
run_files

for (f in run_files){
  print(f)
  rmarkdown::render(f)
} 
