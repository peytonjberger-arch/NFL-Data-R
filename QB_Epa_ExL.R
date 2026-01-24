library(nflfastR)
library(ggrepel)
library(tidyverse)



pbp <- load_pbp(2025)


qbs_early <- pbp %>% 
  filter(pass == 1, down %in% c(1,2), season_type == "REG") %>% 
  group_by(passer_player_name, passer_player_id, posteam) %>% 
  summarise(
    epa_early = mean(epa),
    plays_early = n()
  )



qbs_late <- pbp %>% 
  filter(pass == 1, down %in% c(3,4), season_type == "REG") %>% 
  group_by(passer_player_name, passer_player_id, posteam) %>% 
  summarise(
    epa_late = mean(epa),
    plays_late = n()
  ) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))


qbs_downs <- qbs_late %>% 
  left_join(qbs_early, by = "passer_player_name" ) %>% 
  filter(plays_early >= 100)


ggplot(qbs_downs)+
  geom_point(aes(x = epa_early, y = epa_late), color = qbs_downs$team_color)+
  geom_text_repel(aes(x = epa_early, y = epa_late, label = passer_player_name))+
  geom_hline(yintercept = mean(qbs_downs$epa_late), color = "red", lty = "dashed")+
  geom_vline(xintercept = mean(qbs_downs$epa_early), color = "red", lty = "dashed")+
  theme_bw()+
  labs(
    title = "QB Early Downs vs Late Downs EPA in 2025",
    subtitle = "Min 100 Dropbacks - Regular Season",
    x = "Epa per Dropback on Early Downs",
    y = "Epa per Dropback on Late Downs",
    caption = "By Peyton Berger | Data: nflfastR"
  )+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))

#Save Scatterplot
ggsave("SP_EPA_ExL.png", width = 15, height = 10, dpi = "retina")  
  
  
  
  

  
