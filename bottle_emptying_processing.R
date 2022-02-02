library(tidyverse)

cp = read.csv("CPcoded.csv") # Originally "Coder Bottle Emptying Data" - put in 1 sheet, added Dyad_set column and converted to CSV
uom = read.csv("UOMcoded.csv") # Originally "Typical Feeding - Bottle Weight Amt and rate" - converted to CSV

#University of Michigan's precise coding - adjusting the time points to match with our codes.
#- neonur_bot1_pre: bottle weight before feeding
#- neonur_bot1_post: bottle weight after feeding
uom <-  uom %>% 
  mutate(Dyad_set = if_else(
    timepoint == 1, "2wkB",
    paste(timepoint, "moB", sep = ""),
  )) %>% 
  select(-timepoint)

cp <- cp %>% 
  mutate(Observations = Observations %>% 
           str_replace_all(" ?[mM]o[bB]", "moB") %>% #fixing frequent typos in "xmoB" - space is optional, capitalization is too.
           str_replace_all("  ", " ")) %>% 
  separate(col = Observations, into = c("id", "Dyad_set", "Coder")) %>% 
  mutate(id = as.integer(id))

combined <- cp %>% left_join(uom, by = c("id", "Dyad_set"))



#Verifying distinct values and data integrity
combined %>% distinct(Bottle.Emptying)
combined %>% distinct(Dyad_set)
combined %>% #This plot reveals possible change in baseline bottle weight?
  pivot_longer(cols = c(neonur_bot1_pre,
                        neonur_bot2_pre,
                        neonur_bot3_pre,
                        neonur_bot1_post,
                        neonur_bot2_post,
                        neonur_bot3_post), values_to = "Weight", names_to = "bottle_num") %>% 
  ggplot(aes(x = id, y = Weight, col = bottle_num)) +
  geom_point() +
  facet_grid(~Dyad_set)

write.csv(combined, file = "combined_emptying.csv")
