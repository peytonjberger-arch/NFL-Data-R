library(nflfastR)
library(ggrepel)
library(tidyverse)



pbp <- load_pbp(2025)



rbs <- pbp %>% 
  filter(rush == 1, season_type == "REG") %>% 
  group_by(rusher_player_id, rusher_player_name, posteam) %>% 
  summarise(
    epa = mean(epa, na.rm = T),
    plays = n()
  ) %>% 
  filter(plays >= 100) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

  
  ggplot(rbs)+
    geom_point(aes(x = epa, y = plays), color = rbs$team_color)+
    geom_text_repel(aes(x = epa, y = plays, label = rusher_player_name))+
    geom_hline(yintercept = mean(rbs$plays), color = "red", lty = "dashed")+
    geom_vline(xintercept = mean(rbs$epa), color = "red", lty = "dashed")+
    theme_bw()+
    labs(
      title = "RB Rushing Efficiency vs Volume",
      subtitle = "Min 100 Rushing Attempts - Regular Season",
      x = "EPA per Rush",
      y = "Rushing Attempts",
      caption = "Peyton Berger | Data: nflfastR"
    )+
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
    theme(plot.subtitle = element_text(size = 12, hjust = 0.5))

  #Save Scatterplot
  ggsave("SP_RB_EffxVol.png", width = 15, height = 10, dpi = "retina")
  