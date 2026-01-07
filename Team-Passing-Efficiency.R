library(nflfastR)
library(tidyverse)
library(ggimage)



pbp <- load_pbp(2025)


#Define Data Frame
team_epa <- pbp %>% 
  filter(pass == 1, !is.na(epa), season_type == "REG") %>%
  filter(qb_scramble == 0) %>% 
  group_by(posteam) %>% 
  summarise(
    EPA = mean(epa),
    dropbacks = n()
  ) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

  

#Create a Scatterplot
ggplot(team_epa, aes(x = EPA, y = dropbacks))+
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9)+
  geom_hline(yintercept = mean(team_epa$dropbacks), color = "red", lty = "dashed")+
  geom_vline(xintercept = mean(team_epa$EPA), color = "red", lty = "dashed")+
  theme_bw()+
  labs(
    title = "NFL Team Passing Efficiency (2025)",
    subtitle = "EPA per Dropback on True Dropbacks — Regular Season",
    x = "EPA per Dropback",
    y = "Total Dropbacks",
    caption = "By Peyton Berger | Data: nflfastR"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))


# Save Chart
ggsave('SP_Team_Passing_Efficiency.png', width = 15, height = 10, dpi = 'retina')



# Create a Bar Chart
ggplot(team_epa)+
  geom_col(mapping = aes(y = reorder(posteam, EPA), x = EPA), color = team_epa$team_color2, fill = team_epa$team_color)+
  geom_image(mapping = aes(y = reorder(posteam, EPA), x = EPA, image = team_logo_espn), asp = 16/9)+
  theme_bw()+
  labs(
    title = "NFL Team Passing Efficiency (2025)",
    subtitle = "EPA per Dropback on True Dropbacks — Regular Season",
    x = "EPA per Dropback",
    y = "Team",
    caption = "By Peyton Berger | Data: nflfastR"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))+
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))


# Save Chart
ggsave('Col_Team_Passing_Efficiency.png', width = 15, height = 10, dpi = 'retina')
