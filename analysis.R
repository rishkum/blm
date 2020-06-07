library(tidyverse)
remotes::install_github("rishkum/dectechviz")
library(dectechViz)
library(lubridate)

df <- read_csv("fatal-police-shootings-data.csv")

r_demo <- read_csv("demo.csv" )
r_demo <- r_demo %>% rename(US_pop = percent)
r_demo$pop <- (r_demo$US_pop * 320000000)

df$Month_Yr <- as.Date(df$date, "%Y-%m")

# In terms of time
p.disparity.time <- df %>% select(date, race) %>% 
  mutate(year = format(date, "%Y")) %>% 
  group_by(year, race)  %>% 
  summarise(total = n()) %>% 
  left_join(., r_demo, by = "race") %>% 
  mutate(permillion = (total/pop)*1000000) %>% 
  filter(!is.na(race)) %>% filter(!(year == 2020)) %>% 
  filter(!(race=="N")) %>% ungroup() %>%  mutate(race = case_when(.$race=="A" ~"Asians",
                                            .$race=="B" ~ "Blacks",
                                            .$race=="H" ~"Hispanics",
                                            .$race=="O"~ "Others",
                                            .$race=="W"~ "Whites")) %>% 
  ggplot(., aes(x=year, y=permillion, group =race)) +
  geom_point( aes (color = race))+
  geom_smooth(method = "lm",
              se=F,
              aes (color = race, fill = race)) + s
  scale_y_continuous(label = scales::number) +
  scale_fill_brewer(palette = "Paired")

p.disparity.time.pr <- ggpretay(p.disparity.time, title = "Does time make a difference?",
         subtitle = "Overall incidents per million are falling per year, albiet quite slowly",
         yaxis = "Police shootings per million",
         xaxis = "Year") 

p.disparity.time.pr

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

remotes::install_github("larmarange/JLutils")

# Tried labeling for a fill chart = failed :()
test_labels = party_data %>%
  arrange(year, desc(race)) %>%
  group_by(year,party,race) %>%
  summarise(permillion = mean(permillion)) %>% 
  mutate(ylabel_pos = cumsum(permillion)/sum(permillion),
         ylabel = permillion/sum(permillion)) %>%
  group_by(race, add = TRUE) %>%
  mutate(ylabel = sum(ylabel)) %>%
  slice(n())


# Party Matters?
party_data <- df %>% select(date, state, race) %>% 
  mutate(year = format(date, "%Y")) %>% 
  group_by(year, state, race)  %>% 
  summarise(total = n()) %>% filter(year >= 2016) %>% 
  left_join(., election, by = "state") %>% 
  left_join(., r_demo, by = "race") %>% 
  group_by(party, year, race) %>% summarise(permillion = sum(total)) %>% 
  left_join(., r_demo, by = "race") %>% 
  mutate(permillion = (permillion/pop)*1000000) %>% 
  filter(!(is.na(race)))

p.party.diff <- party_data %>% filter(race == "B") %>% 
  ggplot(., aes( x = year, y = permillion, fill = party)) +
    geom_col(position = "dodge", ) +
  scale_fill_brewer(palette="Paired") + guides(title = " State Governemnt Run by") 
    

p.party.diff.pr <- ggpretay(p.party.diff, title = "Party matters?",
         subtitle = "Falities of black people are higher by police in Republican run states",
         yaxis = "Black persons shot by police per million",
         xaxis = "Year") 


p.party.diff.pr +  theme(axis.text= element_text(family ="Muli"))
  

  
summary(lm(permillion ~ party + race, party_data)   )





+ stat_stack_labels()

  geom_text(aes(label=paste0(sprintf("%1.1f", permillion*100),"%")), 
            position=position_stack(vjust=0.5), colour="white")

  