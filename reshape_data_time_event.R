
# data <- read_csv("all_data.csv") %>% 
#   rename("id" = "ID_Number",
#          "coder_id" = "Coder") %>%
#   mutate(
#     Time_Relative_sf = as.double(Time_Relative_sf),
#     Duration_sf = as.double(Duration_sf),
#     Comment = as.logical(Comment),
#     id = factor(id),
#     Position_of_Infant = as.integer(Position_of_Infant),
#     time = as.double(time),
#     duration = as.double(duration),
#     dyad_set = factor(dyad_set, levels = dyad_set_levels),
#     Bottle_Rejector = replace_na(Bottle_Rejector, 0),
#     Bottle_Rejector = factor(Bottle_Rejector)
#   ) %>% 
#   mutate(Behavior = replace(Behavior, Behavior == "Blocks mouth", "Blocks mouth with hands or feet"))

behaviors <- data %>%
  select(Behavior) %>%
  unique()

# Vectorized sequence function
# seq_vec(x, y) = c(x[1]:y[1], x[2]:y[2], ...)
seq_vec <- Vectorize(seq.default, vectorize.args = c("from", "to"))

bottle_states <- data %>%
  filter(str_detect(Behavior, "^Bottle")) %>%
  pull(Behavior) %>%
  unique()

create_state_path <- function(x) {
  
  y <- rep(0, length(x))
  start_rows <- which(x == "State start")
  stop_rows <- which(x == "State stop") - 1
  if(length(start_rows) != length(stop_rows)){
    stop("Unequal lengths of start and stop vectors")
  }
  state_rows <- unlist(seq_vec(from = start_rows, to = stop_rows))
  y[state_rows] <- 1
  as.numeric(y)
  
}

data <- data %>%
  arrange(dyad_set, id, time, desc(Event_Type))

for (l in bottle_states) {
  
  x = ifelse(data$Behavior == l, data$Event_Type, NA)
  y = create_state_path(x)
  data <- data %>%
    add_column(!!l := y)
  
}

bottle_state_code = max.col(data %>%
                              select(starts_with("Bottle "))) *
  (data %>% mutate(bottle_sum = rowSums(across(starts_with("Bottle ")))) %>% pull(bottle_sum) > 0)
bottle_state_code[bottle_state_code == 0] <- NA

data <- data %>%
  mutate(Bottle_state = names(data %>% select(starts_with("Bottle ")))[bottle_state_code]) %>%
  select(-starts_with("Bottle "))

# Function translates start/stop column to 0/1 at each time point

create_state_path <- function(x){
  y <- rep(0, length(x))
  start_rows <- which(x == "State start")
  stop_rows <- which(x == "State stop") - 1
  state_rows <- unlist(seq_vec(from = start_rows, to = stop_rows))
  y[state_rows] <- 1
  as.numeric(y)
}

# spread to create binary column for each behavior
# convert Crying and Eyes Closed to point behaviors first

convert_to_point <- c("Crying", "Eyes Closed")

data_time_event <- data %>%
  filter(!(Behavior %in% convert_to_point & #Remove "State stop" lines for crying and eyes closed
             Event_Type == "State stop")) %>%
  mutate(Event_Type =
           if_else(Behavior %in% convert_to_point, "State point", #Convert crying and eyes closed to points
                   Event_Type)) %>%
  mutate(duration =
           if_else(Behavior %in% convert_to_point, 0, # Should 0 be replaced with state_point_duration?
                   duration)) %>%
  arrange(dyad_set, id, time, desc(Event_Type)) %>%
  pivot_wider(names_from = Behavior,
              values_from = Event_Type)

# Change start/stop to 1/0 and point behaviors to 1/0
# Should be able to do the following with mutate?
# mutate(across(any_of(state_behaviors)), create_state_path)

for (l in setdiff(state_behaviors, convert_to_point)) {
  
  y = create_state_path(data_time_event[, l] %>% pull())
  data_time_event <- data_time_event %>%
    select(-l) %>% 
    add_column(!!l := y)
  
}

for (l in c(point_behaviors, convert_to_point)) { #This repeats eyes closed and is crying twice, though it doesn't make a difference
  if(l %in% colnames(data_time_event)){
    y = data_time_event[, l] %>% pull() %>% 
      replace_na(0) %>% 
      str_replace("State point", "1") %>% 
      #is.na() %>% 
      as.numeric()
    
    data_time_event <- data_time_event %>%
      select(-l) %>% 
      add_column(!!l := y)
    #add_column(!!l := 1 - y)
  }

  
}

# Some times have multiple rows for start/stops for multiple behaviors
# Need to collapse so only one row for each time
# **Only keep last row for each time**
# after all start/stops at a single time have been accounted

# Adjusting times with multiple rows probably needs to be checked more thoroughly

data_time_event <- data_time_event %>%
  group_by(dyad_set, id, time) %>%
  mutate(duration = ifelse(time == 0 & coder_id == "KJR",
                           duration,
                           max(duration))) %>%
  filter(row_number() == n()) %>%
  arrange(dyad_set, id, time) %>%
  mutate(infant_cue = rowSums(across(any_of(cues)))) %>%
  mutate(infant_cue = ifelse(infant_cue > 0, 1, 0)) %>%
  group_by(dyad_set, id) %>%
  select(dyad_set, id, dyad_unit, time, duration,
         starts_with("Bottle"), any_of(c(state_behaviors, point_behaviors)),
         infant_cue) 

# Add counter for each bottle state in a feeding


data_time_event <- data_time_event %>%
  rename_with(~ gsub(" ", "_", .x), starts_with("Bottle ")) %>%
  mutate(Bottle_state_code =
           1 * Bottle_not_in_mouth +
           2 * Bottle_in_mouth +
           3 * Bottle_offered) %>%
  mutate(prev_Bottle_state_code =
           dplyr::lag(Bottle_state_code, n = 1, default = NA)) %>%
  mutate(Bottle_state_change = 
           ifelse(is.na(prev_Bottle_state_code),
                  1,
                  as.numeric(!(Bottle_state_code == prev_Bottle_state_code)))) %>%
  mutate(Bottle_state_counter = cumsum(Bottle_state_change)) %>%
  mutate(next_time =
           dplyr::lead(time, n = 1, default = NA)) %>%
  mutate(time_until_next = 
           ifelse(is.na(next_time), 0, next_time - time)) %>%
  ungroup() %>%
  group_by(dyad_set, id, Bottle_state_counter, Bottle_state) %>%
  mutate(time_in_bottle_state =
           time - min(time)) %>%
  mutate(time_remaining_in_bottle_state =
           max(next_time) - time) %>%
  mutate(time_fraction_bottle_state =
           (time - min(time)) / (max(next_time) - min(time)))


# create a column for the cue that happens at the time
# For now just record multiple cues as "multiple"

cue_columns_only <- data_time_event %>% ungroup() %>% select(any_of(cues))

data_time_event <- data_time_event %>%
  mutate(num_cues_at_time = rowSums(across(any_of(cues)))) %>%
  add_column(cue_at_time = colnames(cue_columns_only)[
    apply(cue_columns_only, 1, which.max)]) %>%
  mutate(cue_at_time = ifelse(num_cues_at_time == 0, NA, cue_at_time)) %>%
  mutate(cue_at_time = ifelse(num_cues_at_time > 1, "multiple", cue_at_time))

write.csv(data_time_event,
          "data_time_event.csv",
          row.names = FALSE)

