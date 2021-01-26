library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2021, week = 5)

d <- tuesdata$plastics

# From all represented countries, which 10 companies are most often in the top 10 
# of most common plastic pollution product sources?

d %>% 
  mutate_if(is.character, tolower) %>% 
  mutate_if(is.character, function(x) {iconv(x, to="ASCII//TRANSLIT")})%>% 
  filter(!(parent_company %in% c("grand total", "unbranded", "null"))) %>% 
  group_by(country, parent_company) %>% 
  summarise(n=sum(grand_total)) %>% 
  group_by(country) %>% 
  top_n(n=10) %>% 
  group_by(parent_company) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  ggplot(aes(x=reorder(parent_company, -n, sum), y=n)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

