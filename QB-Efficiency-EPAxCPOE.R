library(nflfastR)
library(ggplot2)
library(tidyverse)
library(ggimage)
library(ggrepel)
library(gt)
library(teamcolors)
library(nflplotR)


pbp <- load_pbp(2025)


qbs <- pbp %>% 
  filter(season_type == "REG", !is.na(epa)) %>% 
  group_by(name, id) %>% 
  summarise(
    team = first(posteam),
    epa_per_play = mean(epa),
    cpoe_pass = mean(cpoe, na.rm = T),
    dropbacks = sum(pass),
    n_plays = n()
  ) %>% 
  filter(dropbacks > 250) %>% 
  left_join(teams_colors_logos, by = c("team" = "team_abbr")) %>% 
  arrange(desc(epa_per_play))



ggplot(qbs)+
  geom_point(aes(x = epa_per_play, y = cpoe_pass), color = qbs$team_color)+
  geom_text_repel(aes(x = epa_per_play, y = cpoe_pass, label = name))+
  geom_hline(yintercept = mean(qbs$cpoe_pass), color = "red", lty = "dashed")+
  geom_vline(xintercept = mean(qbs$epa_per_play), color = "red", lty = "dashed")+
  theme_bw()+
  labs(
    title = "Quarterback Efficiency: EPA/Play vs. CPOE",
    subtitle = "2025 NFL Regular Season Performance (Min 250 Dropbacks)",
    x = 'QB EPA per Play',
    y = 'QB CPOE per Pass',
    caption = 'By: Peyton Berger | Data: nflfastR'
  )+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  theme(plot.subtitle = element_text(size = 11, hjust = 0.5))
  
  
  