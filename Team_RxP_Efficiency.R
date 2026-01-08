library(nflfastR)
library(tidyverse)
library(ggimage)



pbp <- load_pbp(2025)


epa_p <- pbp %>% 
  filter(pass == 1, qb_scramble == 0, !is.na(epa), season_type == "REG") %>% 
  group_by(posteam) %>% 
  summarise(
    EPA_per_pass = mean(epa),
    dropbacks = n()
  )


epa_r <- pbp %>% 
  filter(rush == 1, qb_scramble == 0, !is.na(epa), season_type == "REG") %>% 
  group_by(posteam) %>% 
  summarise(
    EPA_per_rush = mean(epa),
    rush_attempts = sum(rush)
  ) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))



epa_rp <- epa_p %>% 
  left_join(epa_r, by = c("posteam" = "posteam")) %>%
  group_by(posteam) %>% 
  mutate(Total_EPA = sum(EPA_per_pass, EPA_per_rush))
  
  
  


ggplot(epa_rp, aes(x = EPA_per_rush, y = EPA_per_pass))+
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9)+
  geom_hline(yintercept = mean(epa_rp$EPA_per_pass), color = "red", lty = "dashed")+
  geom_vline(xintercept = mean(epa_rp$EPA_per_rush), color = "red", lty = "dashed")+
  theme_bw()+
  labs(
    title = "NFL Team Offensive Efficiency (2025)",
    subtitle = "On Called Rushes and Passes (Excluding QB Scrambles) — Regular Season",
    x = "EPA per Rush",
    y = "EPA per Pass",
    caption = "By Peyton Berger | Data: nflfastR"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))


# Save Graph
ggsave("SP_Team_RxP_Efficiency.png", width = 15, height = 10, dpi = "retina")



ggplot(epa_rp)+
  geom_col(aes(y = reorder(posteam, Total_EPA), x = Total_EPA), color = epa_rp$team_color2, fill = epa_rp$team_color)+
  geom_image(aes(y = reorder(posteam, Total_EPA), x = Total_EPA, image = team_logo_espn), size = 0.05, asp = 16/9)+
  theme_bw()+
  labs(
    title = "NFL Team Offensive Efficiency (2025)",
    subtitle = "On Called Rushes and Passes (Excluding QB Scrambles) — Regular Season",
    x = "Total Offensive EPA",
    y = "Team",
    caption = "By Peyton Berger | Data: nflfastR"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))


# Save Graph
ggsave("Col_Team_RxP_Efficiency.png", width = 15, height = 10, dpi = "retina")
