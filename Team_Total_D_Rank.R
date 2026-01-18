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
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr"))


ggplot(epa_defense, aes(x = EPA_Pass, y = EPA_Run))+
  geom_image(image = epa_defense$team_logo_espn, size = 0.05, asp = 16/9)+
  geom_hline(yintercept = mean(epa_defense$EPA_Run), color = "red", lty = "dashed")+
  geom_vline(xintercept =mean(epa_defense$EPA_Pass), color = "red", lty = "dashed")+
  scale_x_reverse()+
  scale_y_reverse()+
  theme_bw()+
  labs(
    x = 'Defensive EPA/Pass',
    y = 'Defensive EPA/Run',
    title = 'Total Defense Rank in 2025 Regular Season',
    caption = 'By Peyton Berger | data: nflfastR'
  )+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  annotate(
    "text",
    x = min(epa_defense$EPA_Pass),          
    y = min(epa_defense$EPA_Run),         
    label = "Elite Against Run and Pass", 
    hjust = 1,                  
    vjust = 3,
    color = "green",
    size = 4,
    fontface = "bold"
  )+
  annotate(
    "text",
    x = min(epa_defense$EPA_Pass),          
    y = max(epa_defense$EPA_Run),         
    label = "Elite Against Pass, Bad Against Run", 
    hjust = 1,                  
    vjust = 0,
    color = "blue",
    size = 4,
    fontface = "bold"
  )+
  annotate(
    "text",
    x = max(epa_defense$EPA_Pass),          
    y = min(epa_defense$EPA_Run),         
    label = "Elite Against Run, Bad Against Pass", 
    hjust = 0,                  
    vjust = 3,
    color = "orange",
    size = 4,
    fontface = "bold"
  )+
  annotate(
    "text",
    x = max(epa_defense$EPA_Pass),          
    y = max(epa_defense$EPA_Run),         
    label = "Bad Against Run and Pass", 
    hjust = 0,                  
    vjust = 0,
    color = "red",
    size = 4,
    fontface = "bold"
  )
  

# Save Graph
ggsave("SP_Team_Total_D_Rank.png", width = 15, height = 10, dpi = "retina")


