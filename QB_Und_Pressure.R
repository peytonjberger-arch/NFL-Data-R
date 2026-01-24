library(nflfastR)
library(ggrepel)
library(tidyverse)



pbp <- load_pbp(2025)



qbs <-  pbp %>% 
  filter(pass == 1, season_type == "REG", qb_scramble == 0) %>% 
  group_by(passer_player_name, passer_player_id, posteam) %>% 
  summarise(
    Epa = mean(epa, na.rm = T),
    Pressure_Rate = (mean(qb_hit == 1 | sack == 1)),
    Plays = n()
  ) %>% 
  filter(Plays >= 100) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))



ggplot(qbs)+
  geom_point(aes(x = Epa, y = Pressure_Rate), color = qbs$team_color)+
  geom_text_repel(aes(x = Epa, y = Pressure_Rate, label = passer_player_name))+
  geom_hline(yintercept = mean(qbs$Pressure_Rate), color = "red", lty = "dashed")+
  geom_vline(xintercept = mean(qbs$Epa), color = "red", lty = "dashed")+
  theme_bw()+
  labs(
    title = "QB Performance Under Pressure in 2025",
    subtitle = "Min 100 Dropbacks - Regular Season",
    x = "Epa per Dropback",
    y = "Pressure Rate"
  )+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))


#Save Scatterplot
ggsave("SP_QB_Und_Pressure.png", width = 15, height = 10, dpi = "retina")
