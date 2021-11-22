#Does the best team (best pitchers) have the most games won?

#creating dataset with summary of each pitcher
pitch_players <- pitch_full %>%
  group_by(name, Year, pitcherTeam) %>%
  summarize(KBB = mean(KBB),
            KP = mean(KP),
            WinPct = (mean(Wins)/mean(GamesPlayed))*100) %>%
  arrange(Year, pitcherTeam)

#calculating the number of games each player played 
games_played_pitcher <- pitch_full %>%
  group_by(name, Year, pitcherTeam) %>%
  summarize(count_games = n(),
            team_games = mean(GamesPlayed),
            PctPlayed = count_games / team_games)

#joining the player game data
pitch_players <- left_join(pitch_players, games_played_pitcher, by = c("name", "Year", "pitcherTeam"))

#averaging pitching stats for each team
pitch_players <- pitch_players %>%
  mutate(weighted_KBB = PctPlayed*KBB, weighted_KP = PctPlayed*KP)

#final dataset for linear model
pitcher_success_team <- pitch_players %>%
  group_by(Year, pitcherTeam) %>%
  summarize(avg_wtd_KBB = mean(weighted_KBB), avg_wtd_KP = mean(weighted_KP), WinPct = mean(WinPct))

#creating linear model
team_pitching_model <- lm(WinPct ~ avg_wtd_KBB + avg_wtd_KP, data=pitcher_success_team)
#seeing if linear model is appropriate
plot(team_pitching_model)
#all plots look good, linear model is a good fit
lm_sum <- summary(team_pitching_model)

#creating single variable models for the plots
KP_model <- lm(WinPct ~ avg_wtd_KP, data=pitcher_success_team)
KP_lm_sum <- summary(KP_model)

KBB_model <- lm(WinPct ~ avg_wtd_KBB, data=pitcher_success_team)
KBB_lm_sum <- summary(KBB_model)

#creating two plots for each individual variable
pitcher_success_team %>%
  ggplot(aes(avg_wtd_KP, WinPct, color=pitcherTeam)) +
  geom_point(alpha=0.5, size=2) +
  geom_abline(aes(intercept=KP_lm_sum$coefficients[1,1], slope=KP_lm_sum$coefficients[2,1])) + 
  labs(y="Team Win Percentage", x="Team Average Weighted KP")

pitcher_success_team %>%
  ggplot(aes(avg_wtd_KBB, WinPct, color=pitcherTeam)) +
  geom_point(alpha=0.5, size=2) +
  geom_abline(aes(intercept=KBB_lm_sum$coefficients[1,1], slope=KBB_lm_sum$coefficients[2,1])) + 
  labs(y="Team Win Percentage", x="Team Average Weighted KBB")


