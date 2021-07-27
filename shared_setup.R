
# Shared setup for analysis files.
# If this is not running properly, try running data-reshaping to generate new files to read.


# Libraries

library(readr)
library(dplyr)
library(tidyr) # need devtools version ?
library(tibble)
library(ggplot2)
library(GGally)
library(stringr)
library(purrr)
library(forcats)
library(knitr)
library(purrr)
library(ggpubr)
library(cowplot)
library(gridExtra)

library(gifski)
library(gganimate)
# devtools::install_github('haleyjeppson/ggmosaic')
library(ggmosaic)
library(plotly)

knitr::opts_chunk$set(echo = FALSE,
                      message = FALSE,
                      warning = FALSE)

# Load data and select non-rejectors c(0), rejectors c(1), or all c(0, 1)

if(!exists("include_rejectors")) #Default to only accept non-rejectors
  include_rejectors <- c(0)

# Reading data_duration -------------------------------------------------------

data_duration <- read.csv("data_duration.csv")

dyad_set_levels <- c("2wkB", "2moB", "4moB", "6moB")
n_time_points = length(dyad_set_levels) # Not sure if this is used, but it's here to make sure there's no problems

data_duration <- data_duration %>% 
  mutate(dyad_set = factor(dyad_set, levels = dyad_set_levels)) %>%
  filter(Bottle_Rejector %in% include_rejectors)

attr(data_duration$Time_Relative_sf, "label") = "Seconds into feeding"

# Reading data (used in descriptive-statistics-summer2020) -------------------

data <- read_csv("data.csv") %>% 
  # rename("id" = "ID_Number",
  #        "coder_id" = "Coder") %>%
  mutate(
    Time_Relative_sf = as.double(Time_Relative_sf),
    Duration_sf = as.double(Duration_sf),
    Comment = as.logical(Comment),
    id = factor(id),
    Position_of_Infant = as.integer(Position_of_Infant),
    time = as.double(time),
    duration = as.double(duration),
    dyad_set = factor(dyad_set, levels = dyad_set_levels),
    Bottle_Rejector = replace_na(Bottle_Rejector, 0),
    Bottle_Rejector = factor(Bottle_Rejector)
  ) %>% 
  mutate(Behavior = replace(Behavior, Behavior == "Blocks mouth", "Blocks mouth with hands or feet")) %>% 
  filter(Bottle_Rejector %in% include_rejectors)

# Reading data_dyad_total_zeros ----------------------------------------------------

data_dyad_total_zeros <- read_csv("data-dyad-total-zeros.csv") %>%
  mutate(dyad_set = factor(dyad_set, levels = dyad_set_levels),
         id = factor(id),
         Bottle_Rejector = factor(Bottle_Rejector),
         num_per_min = num_per_length * 60) %>%
  filter(Bottle_Rejector %in% include_rejectors)

data_map <- read_csv("data-map.csv") %>%
  mutate(id = factor(id))

if(1 %in% include_rejectors) {
  data_map <- data_map %>%
    mutate(time_points = Time_points_with_rejectors)
} else {
  data_map <- data_map %>%
    mutate(time_points = Time_points)
}

# Creating list variables

behaviors <- data_duration %>%
  select(Behavior) %>%
  unique() %>%
  pull()

state_behaviors <- data_duration %>%
  filter(Event_Type == "State start") %>%
  select(Behavior) %>%
  unique() %>%
  pull()

point_behaviors <- data_duration %>%
  filter(Event_Type == "State point") %>%
  select(Behavior) %>%
  unique() %>%
  pull()

cues <- data_duration %>%
  filter(Grouped_behavior == "Infant Satiation Cues") %>%
  select(Behavior) %>%
  unique() %>%
  pull()

mom_behaviors <- data_duration %>%
  filter(dyad_unit == "Mom") %>%
  select(Behavior) %>%
  unique() %>%
  pull()

bottle_states <- state_behaviors[grep("Bottle", state_behaviors)]

# Dyad set counts (depending on whether bottle rejectors are included) -------------------------------

dyad_set_counts <- data_duration %>% 
  distinct(dyad_set, id) %>% 
  group_by(dyad_set) %>% 
  summarise(count = n())

# Setting ggplot automation --------------------------------------------------------

theme_set(theme_minimal())

cbPalette  <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
ggplot <- function(...) ggplot2::ggplot(...) + 
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette)

# Occasionally used simple items --------------------------------------------

state_point_duration <- 0.00001 # for easier coding, treat state points as start/stop

# Behavior codebook - currently used in descriptive-statistics-summer2020, but good to have.

behavior_codebook <- read_csv("behavior_codebook.csv")

# get_time_split arbitrary quantile function --------------------------------------


get_time_split <- function(x, q) {
  # x is a number in [0, 1]
  # q is number of time splits, e.g., 4 for quarters, 2 for halves, etc
  ifelse(x == 1, q, 1 + trunc((x * q) %% q))
}

# Ratio lines (used in descriptive-statistics-infant-cues)

ratio_lines = seq(1, 4, 0.5)

# Auto-labeling ----------------------------------------------------
#Code adapted and improved from: https://www.pogol.net/using-attr-labels-for-ggplot
# Though interesting, it ended up not being worth the effort because we very rarely use an unmodified dataset to create plots.



# ggplot=function(...){
#   plot=ggplot2::ggplot(...) +
#     scale_fill_manual(values = cbPalette) # Add the color palette
#   dat=plot$data
#   for(m in names(plot$mapping)){
#     char=paste0(plot$mapping[m] %>%
#                   str_remove("~")
#                   )
#     if(char %in% colnames(dat)){
#       ml=attr(dat[,char],"label")
#       plot$labels[m]=ml
#     }
# 
#   }
# 
#   plot
# }

# Setting automatic labels --------------------------------------------------

# set_label = function(df, var, label){
#   attr(df[,var], "label") <- label
#   return(df)
# }
# 
# data_duration <- data_duration %>%
#   set_label("Duration_sf", "Duration") %>%
#   set_label("Time_Relative_sf", "Seconds into feeding")

#attr(data_duration$Time_Relative_sf, "label") = "Seconds into feeding"

