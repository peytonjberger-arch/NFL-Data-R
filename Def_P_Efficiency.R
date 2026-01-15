library(nflfastR)
library(tidyverse)
library(ggimage)


pbp <-load_pbp(2025)



def_epa_p <- pbp %>% 
  filter(pass == 1, !is.na(epa), season_type == "REG") %>% 
  group_by(defteam) %>% 
  summarise(
    epa = mean(epa),
    plays = n()
  ) %>% 
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr"))


ggplot(def_epa_p, aes(x = epa, y = plays))+
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9)+
  geom_hline(yintercept = mean(def_epa_p$plays), color = "red", lty = "dashed")+
  geom_vline(xintercept = mean(def_epa_p$epa), color = "red", lty = "dashed")+
  scale_x_reverse()+
  theme_bw()+
  labs(
    title = "NFL Defensive Efficiency Against the Pass (2025)",
    subtitle = "Defensive EPA Against the Pass — Regular Season",
    x = "Defensive EPA Against the Pass",
    y = "Total Pass Attempts",
    caption = "By Peyton Berger | Data: nflfastR"
  )+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))


#Save Graph
ggsave("SP_Def_Pass_Efficiency.png", width = 15, height = 10, dpi = "retina")
