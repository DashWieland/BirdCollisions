library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(patchwork)
library(ggthemes)

bird_collisions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")

### recoding some variables for all plots
bird_collisions <- bird_collisions %>%
  mutate(location = case_when(
    locality == "CHI" ~ "Elsewhere in Chicago",
    locality == "MP" ~ "McCormick Place"
  ))

### Data for p1 - all collisions by year 
total <- bird_collisions %>%
  mutate(year = year(ymd(date))) %>%
  group_by(year) %>%
  mutate(collisions = n()) %>%
  select(year, collisions) %>%
  unique()

### Data for p2 - all collisions by year and location
total_local <- bird_collisions %>%
  mutate(year = year(ymd(date))) %>%
  group_by(year, location) %>%
  mutate(collisions = n()) %>%
  select(year, location, collisions) %>%
  unique()

### Data for p3 - all collisions by month, year, and location
month_year <- bird_collisions %>%
  mutate(lubdate = ymd(date),
         year = year(lubdate),
         monthofyear = month(lubdate,
                             label = TRUE,
                             abbr = TRUE)) %>%
  group_by(year, monthofyear, location) %>%
  mutate(collisions = n())

### p1 - all collisions by year 
p1 <- ggplot(data = total,
             aes(x = year,
                 y = collisions)) +
  geom_line(color = "#6F9283",
            size = 1) +
  #geom_point(color = "#6F9283") + 
  theme_fivethirtyeight() + 
  labs(title = "1. Total reported collisions by year",
       subtitle = "There were more collisions reported in 2006-2016 than in earlier years.
The trend of the data changed in the early 2000s as well")

### p2 - all collisions by year and location
p2 <- ggplot(data = total_local,
             aes(x = year,
                 y = collisions,
                 group = location)) +
  geom_vline(xintercept=2002,
             colour="#C7958D",
             size = 2) +
  geom_line(aes(color = location),
            size = 1) +
  #geom_point(aes(color = location)) + 
  ylim(0, 4000) +
  scale_color_manual(values=c("#6F9283", "#654C4F")) +
  theme_fivethirtyeight() + 
  labs(title = "2. Reported collisions by year and location",
       subtitle = "Collisions at McCormick Place have generally decreased, 
but collisions elsewhere in Chicago have greatly increased.") +
  annotate("label",
           x = 1978,
           y = 3800,
           fill = "#F0F0F0",
           alpha = .75,
           label.size = NA,
           color = "#3C3C3C",
           label = "3. Cause of increase in reports:",
           fontface = 2,
           size = 5,
           hjust = 0) + 
  annotate("label",
           x = 1978, 
           y = 2900, 
           fill = "#F0F0F0",
           alpha = .75, 
           label.size = NA, 
           color = "#3C3C3C",
           label = "The Chicago Bird Collision Monitors began 
collision monitoring efforts throughout 
downtown Chicago in 2002.",
           hjust = 0) +
  theme(legend.title = element_blank())

### p3 - all collisions by month, year, and location
p3 <- ggplot(data = month_year,
             aes(x=year,
                 y=collisions,
                 group = location)) +
  geom_vline(xintercept=2002,
             colour="#C7958D",
             size = 2) +
  geom_line(aes(color = location),
            size = 1) +
  scale_color_manual(values=c("#6F9283", "#654C4F")) +
  facet_wrap("monthofyear", ncol=4) + 
  theme_fivethirtyeight() + 
  labs(title = "4. Reported collisions by year, location, and month",
       subtitle = "Only collisions during migratory months are recorded. Most of the new collision reports are from May, September, and October
of each year and from around Chicago, not McCormick Place. The conclusion? Collisions may not be increasing, but reporting is.") +
  theme(legend.title = element_blank())
  


p_assemble <- (p1 | p2) / p3
p_assemble + 
  plot_annotation(title = "Are more birds colliding with buildings in Chicago?",
                  subtitle = "Probably not, but more collisions are being reported.",
                  caption = "Winger BM, Weeks BC, Farnsworth A, Jones AW, Hennen M, Willard DE (2019)
                  Nocturnal flight-calling behaviour predicts vulnerability to artificial light in migratory birds. 
                  Proceedings of the Royal Society B 286(1900): 20190364. 
                  https://doi.org/10.1098/rspb.2019.0364",
                  theme = theme_fivethirtyeight())
                  

ggsave("collisions.png", width = 13, height = 10, units = c("in"))

