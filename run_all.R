
library(tidyverse)

files <- list.files()

files_df <- data.frame(file_name = files) %>% 
  filter(str_detect(file_name, ".Rmd")) %>% 
  filter(file_name != "data-reshaping.Rmd")

run_files <- files_df$file_name

for (f in run_files){
  print(f)
  rmarkdown::render(f)
} 
