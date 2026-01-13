library(nflfastR)
library(tidyverse)
library(ggimage)


pbp <- load_pbp(2024:2025)

# Define data frame for all rushing and passing plays
pbp_rp <- pbp %>% 
  filter(pass == 1 | rush == 1) %>% 
  filter(!is.na(epa))

# Create new df filtering for 2024 regular season
offense_24 <- pbp_rp %>% 
  filter(season == 2024, season_type == "REG") %>% 
  group_by(posteam) %>% 
  summarise(
    EPA_24 = mean(epa)
  )

# Create new df filtering for 2025 regular season
offense_25 <- pbp_rp %>% 
  filter(season == 2025, season_type == "REG") %>% 
  group_by(posteam) %>% 
  summarise(
    EPA_25 = mean(epa)
  )

# Create new df joining 2024 and 2025 dfs together
offense_all <- offense_24 %>% 
  left_join(offense_25, by = "posteam") %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))



# Create scatterplot
ggplot(offense_all, aes(x = EPA_24, y = EPA_25))+
  geom_image(image = offense_all$team_logo_espn, size = 0.05, asp = 16/9)+
  geom_hline(yintercept = mean(offense_all$EPA_25), color = "red", lty = "dashed")+
  geom_vline(xintercept = mean(offense_all$EPA_24), color = "red", lty = "dashed")+
  theme_bw()+
  labs(
    x = 'Offensive EPA/Play in 2024',
    y = 'Offensive EPA/Play in 2025',
    title = 'Offensive EPA/Play in 2024 Compared to 2025',
    caption = 'By Peyton Berger | data: nflfastR'
  )+
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  annotate(
    "text",
    x = max(offense_all$EPA_24),          
    y = max(offense_all$EPA_25),         
    label = "Elite O in '24\nElite O in '25", 
    hjust = 1,                  
    vjust = 3,
    color = "green",
    size = 4,
    fontface = "bold"
  )+
  annotate(
    "text",
    x = max(offense_all$EPA_24),          
    y = min(offense_all$EPA_25),,         
    label = "Elite O in '24\nBad O in '25", 
    hjust = 1,                  
    vjust = -2,
    color = "blue",
    size = 4,
    fontface = "bold"
  )+
  annotate(
    "text",
    x = min(offense_all$EPA_24),          
    y = max(offense_all$EPA_25),,         
    label = "Bad O in '24\nElite O in '25", 
    hjust = 0,                  
    vjust = 3,
    color = "orange",
    size = 4,
    fontface = "bold"
  )+
  annotate(
    "text",
    x = min(offense_all$EPA_24),          
    y = min(offense_all$EPA_25),         
    label = "Bad O in '24\nBad O in '25", 
    hjust = 0,                  
    vjust = -2,
    color = "red",
    size = 4,
    fontface = "bold"
  )

#Save Scatterplot
ggsave("SP_OEPA_24x25.png", width = 15, height = 10, dpi = "retina")



