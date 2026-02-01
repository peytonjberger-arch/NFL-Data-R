library(nflfastR)
library(ggrepel)
library(tidyverse)



pbp <- load_pbp(2025)


rbs <- pbp %>%
  filter(rush == 1, season_type == "REG") %>% 
  group_by(rusher_player_name, rusher_player_id, posteam) %>% 
  summarize(
    success = mean(success, na.rm = T),
    explosive_rr = sum(yards_gained >= 10),
    plays = n()
  ) %>% 
  filter(plays >= 100) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))


ggplot(rbs)+
  geom_point(aes(x = success, y = explosive_rr), color = rbs$team_color)+
  geom_text_repel(aes(x = success, y = explosive_rr, label = rbs$rusher_player_name))+
  geom_hline(yintercept = mean(rbs$explosive_rr), color = "red", lty = "dashed")+
  geom_vline(xintercept = mean(rbs$success), color = "red", lty = "dashed")+
  theme_bw()+
  labs(
  title = "RB Success Rate vs Explosiveness",
  subtitle = "Min 100 Rushing Attempts - Regular Season",
  x = "Success Rate",
  y = "Number of Explosive Runs",
  caption = "Peyton Berger | Data: nflfastR"
  )+
 theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
 theme(plot.subtitle = element_text(size = 12, hjust = 0.5))

#Save Scatterplot
ggsave("SP_RB_SRxExpl.png", width = 15, height = 10, dpi = "retina")


