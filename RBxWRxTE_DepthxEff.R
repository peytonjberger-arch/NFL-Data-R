library(nflfastR)
library(ggrepel)
library(tidyverse)



pbp <- load_pbp(2025)


receivers <- pbp %>% 
  filter(pass == 1, season_type == "REG", !is.na(receiver_player_id)) %>% 
  group_by(receiver_player_id, receiver_player_name, posteam) %>% 
  summarize(
    airy = mean(air_yards, na.rm = T),
    epa = mean(epa, na.rm = T),
    plays = n()
  ) %>% 
  filter(plays >= 80) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))


ggplot(receivers)+
  geom_point(aes(x = airy, y = epa), color = receivers$team_color)+
  geom_text_repel(aes(x = airy, y = epa, label = receiver_player_name))+
  geom_hline(yintercept = mean(receivers$epa), color = "red", lty = "dashed")+
  geom_vline(xintercept = mean(receivers$airy), color = "red", lty = "dashed")+
  theme_bw()+
  labs(
    title = "RB/WR/TE Depth vs Efficiency",
    subtitle = "Min 80 Targets - Regular Season",
    x = "Average Air Yards per Reception",
    y = "EPA per Target",
    caption = "Peyton Berger | Data: nflfastR"
  )+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))


#Save Scatterplot
ggsave("SP_RBxWRxTE_DepthxEff.png", width = 15, height = 10, dpi = "retina")
