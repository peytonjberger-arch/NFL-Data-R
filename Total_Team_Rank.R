library(nflfastR)
library(tidyverse)
library(ggimage)

pbp <- load_pbp(2025)


epa_dp <- pbp %>% 
  filter(pass == 1, !is.na(epa), season_type == "REG") %>% 
  group_by(defteam) %>% 
  summarise(
    EPA_Pass = mean(epa)
  )


epa_dr <- pbp %>% 
  filter(rush == 1, !is.na(epa), season_type == "REG") %>% 
  group_by(defteam) %>% 
  summarise(
    EPA_Run = mean(epa)
  )



epa_defense <- epa_dp %>% 
  left_join(epa_dr, by = "defteam") %>% 
  group_by(defteam) %>% 
  summarise(
    total_d_epa = sum(EPA_Run, EPA_Pass)
  )



epa_op <- pbp %>% 
  filter(pass == 1, !is.na(epa), season_type == "REG") %>% 
  group_by(posteam) %>% 
  summarise(
    EPA_per_pass = mean(epa),
    dropbacks = n()
  )


epa_or <- pbp %>% 
  filter(rush == 1, !is.na(epa), season_type == "REG") %>% 
  group_by(posteam) %>% 
  summarise(
    EPA_per_rush = mean(epa),
    rush_attempts = sum(rush)
  )



epa_offense <- epa_op %>% 
  left_join(epa_or, by = c("posteam" = "posteam")) %>% 
  group_by(posteam) %>% 
  summarise(
    total_o_epa = sum(EPA_per_pass, EPA_per_rush)
  )



team_odepa <- epa_offense %>% 
  left_join(epa_defense, by = c("posteam" = "defteam")) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))





ggplot(team_odepa, aes(x = total_o_epa, y = total_d_epa))+
  geom_image(image = team_odepa$team_logo_espn, size = 0.05, asp = 16/9)+
  geom_hline(yintercept = mean(team_odepa$total_d_epa), color = "red", lty = "dashed")+
  geom_vline(xintercept =mean(team_odepa$total_o_epa), color = "red", lty = "dashed")+
  scale_y_reverse()+
  theme_bw()+
  labs(
    x = 'Total Offensive EPA',
    y = 'Total Defensive EPA',
    title = 'Total Team Rank by EPA in 2025 Regular Season',
    caption = 'By Peyton Berger | data: nflfastR'
  )+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

# Save Graph
ggsave("SP_Total_Team_Rank.png", width = 15, height = 10, dpi = "retina")


team_epa <- team_odepa %>%
  group_by(posteam) %>% 
  summarise(
    total_epa = sum(total_o_epa, 1-total_d_epa)) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))


ggplot(team_epa)+
  geom_col(aes(y = reorder(posteam, total_epa), x = total_epa), color = team_epa$team_color2, fill = team_epa$team_color)+
  geom_image(aes(y = reorder(posteam, total_epa), x = total_epa, image = team_logo_espn), size = 0.05, asp = 16/9)+
  theme_bw()+
  labs(
    title = "NFL Total Team Efficiency in Regular Season (2025)",
    x = "Adjusted Team Total EPA",
    y = "Team",
    caption = "By Peyton Berger | Data: nflfastR"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))


# Save Graph
ggsave("Col_Total_Team_Rank.png", width = 15, height = 10, dpi = "retina")



