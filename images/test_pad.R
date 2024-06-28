story_pollen_top5_count <- story_pollen_long %>%
  group_by(variablename) %>%
  mutate(total_count = sum(value)) %>%
  slice(1) %>% 
  ungroup() %>%
  select(variablename, total_count) %>% 
  arrange(desc(total_count)) %>% 
  slice(1:5)

story_top5 <- story_pollen_long %>% 
  filter(variablename %in% c(story_pollen_top5_count$variablename))

story_other <- story_pollen_long %>%
  filter(!variablename %in% c(story_pollen_top5_count$variablename)) %>%
  mutate(variablename = "other") %>%
  group_by(variablename, age) %>%
  summarise(value = sum(value))


story_top5_res <- bind_rows(story_top5, story_other) %>%
  mutate(variablename = factor(variablename, levels =  c("other", rev(story_pollen_top5_count$variablename))))



ggplot(story_top5_res, aes(x = age, y = value)) +
  geom_area(colour = "grey90") +
  geom_col() +
  scale_x_reverse(breaks = scales::breaks_pretty(n = 6)) +
  coord_flip() +
  # ylim(0, 0.5) +
  labs(y = "Pollen counts", x = "Time (ybp)") +
  facet_wrap(~variablename,
             nrow = 1) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
  )


hardwood_names <- c("Carya", "Ostrya/Carpinus", "Acer", "Juglans", "Fraxinus")
target_taxa <- c("Ulmus", "Fagus grandifolia", "Quercus")

story_pollen_long_hardwood <- story_pollen_long %>%
  filter(variablename %in% hardwood_names) %>% 
  mutate(variablename = "hardwood") %>% 
  group_by(siteid, sitename,
           sampleid, variablename, units, age,
           agetype, depth, datasetid,
           long, lat) %>%
  summarise(value = sum(value), .groups='keep')

unique(story_pollen_long_hardwood$variablename)


story_pollen_long_targets <- story_pollen_long %>%
    filter(variablename %in% target_taxa)
unique(story_pollen_long_targets$variablename)


story_other_taxa <- story_pollen_long %>%
  filter(!variablename %in% c(hardwood_names, target_taxa)) %>%
  mutate(variablename = "other") %>% 
  group_by(siteid, sitename,
           sampleid, variablename, units, age,
           agetype, depth, datasetid,
           long, lat) %>%
  summarise(value = sum(value), .groups='keep')


state_variables <- bind_rows(story_other_taxa, story_pollen_long_hardwood, story_pollen_long_targets) %>% 
  mutate(variablename = factor(variablename, levels =  c("other", "hardwood", target_taxa)),
         value = replace_na(value, 0))


ggplot(state_variables, aes(x = age, y = value)) +
  geom_area(colour = "grey90") +
  geom_col() +
  scale_x_reverse(breaks = scales::breaks_pretty(n = 6)) +
  coord_flip() +
  # ylim(0, 0.5) +
  labs(y = "Pollen counts", x = "Time (ybp)") +
  facet_wrap(~variablename,
             nrow = 1) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
  )


state_variables_wide <- state_variables %>%
    pivot_wider(id_cols = c(age, depth), names_from = variablename, values_from = value) %>% 
    arrange(age)


bin_width <- 50
bins <- cut(state_variables_wide$age, 
            breaks = seq(from = min(state_variables_wide$age),
            to = max(state_variables_wide$age + bin_width),
            by = bin_width), include.lowest = TRUE, labels = FALSE)
  

state_variables_bin <- bind_cols(bins = bins, state_variables_wide) %>% 
  group_by(bins) %>% 
  summarise(
    age = mean(age, na.rm = T),
    ocfs = mean(ocfs, na.rm = T),
    char_acc = mean(char_acc, na.rm = T),
    other = sum(other, na.rm = T),
    Grass = sum(Grass, na.rm = T),
    Herbs = sum(Herbs, na.rm = T),
    Pinus = sum(Pinus, na.rm = T),
    Quercus = sum(Quercus, na.rm = T)
  ) %>% 
  arrange(desc(age))