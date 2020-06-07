library(tidyverse)
remotes::install_github("rishkum/dectechviz")
library(dectechViz)
library(lubridate)

df <- read_csv("fatal-police-shootings-data.csv")

r_demo <- read_csv("demo.csv" )
r_demo <- r_demo %>% rename(US_pop = percent)
r_demo$pop <- r_demo$US_pop * 320

df$Month_Yr <- as.Date(df$date, "%Y-%m")

# In terms of time
p.disparity.time <- df %>% select(date, race) %>% 
  mutate(year = format(date, "%Y")) %>% 
  group_by(year, race)  %>% 
  summarise(total = n()) %>% 
  left_join(., r_demo, by = "race") %>% 
  mutate(permillion = total/pop) %>% 
  filter(!is.na(race)) %>% filter(!(year == 2020)) %>% 
  filter(!(race=="N")) %>% 
  ggplot(., aes(x=year, y=permillion, group =race)) +
  geom_point( aes (color = race))+
  geom_smooth(method = "lm",
              se=F,
              aes (color = race, fill = race)) +
  scale_y_continuous(label = number) +
  scale_fill_brewer(palette = "Paired")

ggpretay(p.disparity.time, title = "Does time make a difference?",
         subtitle = "Black people get shot and shot and tasered more relative to their population in the US",
         yaxis = "Incidents per million",
         xaxis = "Year")


# Compare the effects on race vs the overall US population
to_string <- as_labeller(c(`shot` = "Shot", `shot and Tasered` = "Shot and Tasered"))
library(scales)
p.disparity.population <- df %>% select(manner_of_death, race) %>% group_by( manner_of_death, race) %>% 
  summarise(n = n()) %>%
  mutate(Actual_levels = n / sum(n)) %>% left_join(., r_demo, by = "race") %>% 
  mutate(diff = Actual_levels - US_pop) %>% filter(!is.na(race))  %>% ungroup() %>% 
  select(Actual_levels, US_pop, race, manner_of_death) %>% 
  gather(., Actual_levels, US_pop, -race, -manner_of_death) %>% 
  rename(Reference = Actual_levels, Value = US_pop) %>% 
  ggplot(aes(x = race, y = Value, fill = Reference)) +
  geom_col(position = "dodge") + facet_wrap(~manner_of_death, labeller = to_string) +
  scale_fill_brewer(labels = c("Actual Levels", "US Population Levels"),palette="Paired") +
  scale_y_continuous(labels = percent) + coord_flip()

ggpretay(p.disparity.population, title = "Do all races suffer equally?",
         subtitle = "Black people get shot and shot and tasered more relative to their population in the US",
         yaxis = "Proportion of total popualtion",
         xaxis = "Race")

devtools::install_github("kjhealy/socviz")
library(socviz)
# Using maps
library(elections)
data("eldat")
library(maps)
us_states <- map_data("state")
election
names(state.abb) <- state.name   
us_states$state <- state.abb[str_to_sentence(us_states$region)]
election$state <- (election$st)
us_states_elec <- left_join(us_states, election)

  
df %>% select(date, state, race) %>% 
  mutate(year = format(date, "%Y")) %>% 
  group_by(year, state, race)  %>% 
  summarise(total = n()) %>% filter(year >= 2016) %>% 
  left_join(., election, by = "state") %>% 
  left_join(., r_demo, by = "race") %>% 
  mutate(permillion = total/pop) %>% ungroup() %>% 
  select(race, year, permillion, party) %>% filter(!(race=="N")) %>% 
  ggplot(., aes(year, permillion, fill = race, group = race)) +
  geom_bar(stat = "identity",position = "fill") + facet_grid(~party)
  

  