library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2021, week = 2)

d <- tuesdata$transit_cost

# What is the relationship between total length and average 
# length in the given traffic network?

d %>%
  group_by(country) %>% 
  summarise(
    avg_len = mean(length, na.rm = T),
    sum_len = sum(length, na.rm = T)
    ) %>%
  filter(!is.na(country)) %>% 
  ggplot(aes(log(sum_len), avg_len)) + 
  geom_point() + 
  geom_text(aes(label=country), hjust=0, vjust=0)
