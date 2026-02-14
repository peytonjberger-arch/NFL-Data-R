library(nflreadr)
library(tidyverse)
library(gt)
library(gtExtras)
library(webshot2)



pbp <- load_participation(2025, include_pbp = T)


pbp_personnel <- pbp %>% 
  filter(pass == 1 | rush == 1)


unique(pbp$offense_personnel)


pbp_personnel <- pbp_personnel %>% 
  mutate(
    rb = rowSums(cbind(
      as.numeric(str_extract(offense_personnel, "(?<= )\\d+(?= RB)")),
      as.numeric(str_extract(offense_personnel, "(?<= )\\d+(?= FB)"))
    ), na.rm = T),
    
    te = str_extract(offense_personnel, "(?<= )\\d+(?= TE)") %>%
      as.numeric() %>%
      replace_na(0),
    
    personnel = paste0(rb, te)
  )


unique(pbp$personnel)




formations <- pbp_personnel %>% 
  group_by(posteam, personnel) %>%
  summarise(
    Plays = n(),
    YPP = mean(yards_gained, na.rm = T),
    EPA = mean(epa, na.rm = T),
    Success = mean(success, na.rm = T)
  ) %>% 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))




otbl <- formations %>% 
  ungroup() %>% 
  filter(posteam == "ARI") %>% # Change posteam for whatever team you want to see
  select(team_wordmark, personnel, Plays, EPA, Success, YPP) %>% 
  arrange(-Plays) %>% 
  gt() %>%
  gt_img_rows(columns = team_wordmark) %>%
  fmt_number(columns = c(EPA, YPP), decimals = 2) %>% 
  fmt_percent(columns = c(Success), decimals = 2) %>% 
  gt_theme_espn() %>% 
  opt_row_striping() %>% 
  tab_header(title = "Cardinals Offensive Personnel Breakdown in 2025") %>%
  tab_caption(caption = "By Peyton Berger | Data: FTN Data via nflverse") %>% 
  cols_label(
    team_wordmark = "Team",
    personnel = "Personnel",
  )

#Save Scatterplot
ggsave(otbl, "GT_ARI_Pers.png")



# Defense Coverage 
pbp_coverage <- pbp %>% 
  filter(pass == 1)

unique(pbp$defense_coverage_type)



pbp_coverage <- pbp_coverage %>% 
  mutate(defense_coverage_type = case_when(
    defense_coverage_type == "COVER_3" ~ "Cover 3",
    defense_coverage_type == "COVER_4" ~ "Cover 4",
    defense_coverage_type == "COVER_1" ~ "Cover 1",
    defense_coverage_type == "COVER_0" ~ "Cover 0",
    defense_coverage_type == "COVER_9" ~ "Cover 9",
    defense_coverage_type == "COVER_6" ~ "Cover 6",
    defense_coverage_type == "2_MAN" ~ "Cover 2 Man",
    defense_coverage_type == "COMBO" ~ "Combo Coverage",
    TRUE ~ "Other"
  ))


covrates <- pbp_coverage %>% 
  group_by(defteam, defense_coverage_type) %>% 
  filter(defense_coverage_type != "Other") %>% 
  summarise(
    Plays = n(),
    EPA = mean(epa, na.rm = T),
    Success = mean(success, na.rm = T),
    YPP = mean(yards_gained, na.rm = T)
  ) %>% 
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr"))



dtbl <- covrates %>%
  ungroup() %>% 
  filter(defteam == "ARI") %>% # Change defteam for whatever team you want to see
  select(team_wordmark, defense_coverage_type, Plays, EPA, Success, YPP) %>% 
  arrange(-Plays) %>% 
  gt() %>% 
  gt_img_rows(columns = team_wordmark) %>% 
  fmt_number(columns = c(EPA, YPP), decimals = 2) %>% 
  fmt_percent(columns = c(Success), decimals = 2) %>% 
  gt_theme_espn() %>% 
  opt_row_striping() %>% 
  tab_header(title = "Cardinals Defensive Coverage Breakdown in the 2025 Season") %>% 
  cols_label(
    team_wordmark = "Team",
    defense_coverage_type = "Coverage"
  ) %>% 
  tab_caption(caption = "By Peyton Berger | Data: FTN Data via nflverse")

#Save Scatterplot
ggsave(dtbl, "GT_ARI_Cov.png")
