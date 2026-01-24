library(nflfastR)
library(ggrepel)
library(tidyverse)


pbp <- load_pbp(2025)


qbs <- pbp %>% 
  filter(pass == 1, season_type == "REG", complete_pass == 1) %>% 
  group_by(passer_player_name, passer_player_id, posteam) %>% 
  summarise(
    epa = mean(epa, na.rm = T),
    airyards = mean(air_yards, na.rm = T),
    plays = n()
  ) %>% 
  filter(plays >= 100) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))


ggplot(qbs)+
  geom_point(aes(x = airyards, y = epa), color = qbs$team_color)+
  geom_text_repel(aes(x = airyards, y = epa, label = qbs$passer_player_name))+
  geom_hline(yintercept = mean(qbs$epa), color = "red", lty = "dashed")+
  geom_vline(xintercept = mean(qbs$airyards), color = "red", lty = "dashed")+
  theme_bw()+
  labs(
    title = "QB Aggressiveness vs Efficiency on Completed Passes in 2025",
    subtitle = "Min 100 Completions - Regular Season",
    x = "Air Yards",
    y = "QB EPA per Completed Pass"
  )+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))


#Save Scatterplot

ggsave("SP_QB_AggxEff.png", width = 15, height = 10, dpi = "retina")
