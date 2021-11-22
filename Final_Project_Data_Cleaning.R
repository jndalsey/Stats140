##### Data Cleaning for Stat 140 Final Project

# Libraries

library(tidyverse)

##### MLB team records

# this commented out section transformed raw data into data in form we needed
# no need to uncomment and run

# MLB_wins_raw <- read.csv("MLB_wins_raw.csv")
# 
# MLB_wins <- MLB_wins_raw %>%
#   pivot_longer(cols = Arizona.Diamondbacks:Washington.Nationals,
#                names_to = "Team", values_to = "Wins") %>%
#   mutate(Team = str_replace_all(Team, "\\.(?=[A-Z])", " "))
# 
# write.csv(MLB_wins, "MLB_wins.csv", row.names = FALSE)

MLB_wins <- read.csv("MLB_wins.csv")

##### Clean, intact database

load("Matt_Marks_2_MLB_pitching/compiled_pitcherGame.RData")
load("Matt_Marks_2_MLB_pitching/built_pitcherDate_DL_data.RData")

clean_df <- cbind(xdf_full, xfullmx_XY, xfullmx_XY_PxP) %>% 
  filter(pitcherTeam %in% c("Arizona Diamondbacks", "Atlanta Braves",
                            "Baltimore Orioles", "Boston Red Sox",
                            "Chicago Cubs", "Chicago White Sox",
                            "Cincinnati Reds", "Cincinnati Reds",
                            "Cleveland Indians", "Colorado Rockies",
                            "Detroit Tigers", "Houston Astros",
                            "Kansas City Royals", "Los Angeles Angels",
                            "Los Angeles Dodgers", "Miami Marlins",
                            "Milwaukee Brewers", "Minnesota Twins",
                            "New York Mets", "New York Yankees",
                            "Oakland Athletics", "Philadelphia Phillies",
                            "Pittsburgh Pirates", "San Diego Padres",
                            "San Francisco Giants", "Seattle Mariners",
                            "St. Louis Cardinals", "Tampa Bay Rays",
                            "Texas Rangers", "Toronto Blue Jays",
                            "Washington Nationals")) %>% 
  mutate(weightLbs = as.numeric(weightLbs),
         KBB = round(ifelse(W == 0, K, K / W), 3),
         KP = round(ifelse(x == 0, K, K / x), 3),
         Year = as.numeric(substr(agg_Date, 1, 4))) %>% 
  left_join(MLB_wins,
            by = c("Year" = "Year",
                   "pitcherTeam" = "Team"))

# this final version of clean_df has player data and team data

# alldata is all rows of the given data
# it isn't needed for most of the questions and you probably shouldn't be using it

# load("Matt_Marks_2_MLB_pitching/compiled_DL.RData")

# alldata <- cbind(xdf_full, xfullmx_XY,
#                  xfullmx_XY_PxP, xfullmx_XY_xtra,
#                  xfullmx_XY2, xfullmx_XY3) %>%
#   filter(pitcherTeam %in% c("Arizona Diamondbacks", "Atlanta Braves",
#                             "Baltimore Orioles", "Boston Red Sox",
#                             "Chicago Cubs", "Chicago White Sox",
#                             "Cincinnati Reds", "Cincinnati Reds",
#                             "Cleveland Indians", "Colorado Rockies",
#                             "Detroit Tigers", "Houston Astros",
#                             "Kansas City Royals", "Los Angeles Angels",
#                             "Los Angeles Dodgers", "Miami Marlins",
#                             "Milwaukee Brewers", "Minnesota Twins",
#                             "New York Mets", "New York Yankees",
#                             "Oakland Athletics", "Philadelphia Phillies",
#                             "Pittsburgh Pirates", "San Diego Padres",
#                             "San Francisco Giants", "Seattle Mariners",
#                             "St. Louis Cardinals", "Tampa Bay Rays",
#                             "Texas Rangers", "Toronto Blue Jays",
#                             "Washington Nationals")) %>%
#   mutate(weightLbs = as.numeric(weightLbs),
#          KBB = round(ifelse(W == 0, K, K / W), 3),
#          KP = round(ifelse(x == 0, K, K / x), 3),
#          Year = as.numeric(substr(agg_Date, 1, 4))) %>% 
#   left_join(MLB_wins,
#             by = c("Year" = "Year",
#                    "pitcherTeam" = "Team"))
  
##### Creating Pitch Records

clean_pitches <- clean_df %>% 
  filter(!Ptype_seq %>% str_detect("NA;NA;NA")) %>% 
  filter(Ptype_seq != "NA;NA")

raw_pitch_record <- clean_pitches %>% 
  select(agg_PID, agg_Date, x0_seq:isPitch_seq)

# used this to find how many rows we would need
# hist(sapply(str_match_all(raw_pitch_record$x0_seq, ";"), nrow))
# the number is 40 as the 3rd Quartile is 39
# what this means is that if someone pitches more than 40 pitches we drop all of them after the 40th

pitch_record <- matrix(NA, nrow = nrow(raw_pitch_record), ncol = 5 * 40)
colnames(pitch_record) <- c(paste0("x0_", 1:40), 
                            paste0("z0_", 1:40),
                            paste0("SS_", 1:40),
                            paste0("Ptype_", 1:40),
                            paste0("isPitch_", 1:40))

for (i in 1:nrow(raw_pitch_record)) {
  # unzips x0 values
  pitch_record[i, 1:40] <- unlist(str_split(raw_pitch_record$x0_seq[i], ";"))[1:40]
  # unzips z0 values
  pitch_record[i, 41:80] <- unlist(str_split(raw_pitch_record$z0_seq[i], ";"))[1:40]
  # unzips SS values
  pitch_record[i, 81:120] <- unlist(str_split(raw_pitch_record$SS_seq[i], ";"))[1:40]
  # unzips Ptype values
  pitch_record[i, 121:160] <- unlist(str_split(raw_pitch_record$Ptype_seq[i], ";"))[1:40]
  # unzips isPitch values
  pitch_record[i, 161:200] <- unlist(str_split(raw_pitch_record$isPitch_seq[i], ";"))[1:40]
  
  # reports where loop is at
  if (i %% 10000 == 0) {
    cat("On row:", i, "\n")
  }
}

pitch_record <- as.data.frame(pitch_record)

pr_2 <- pitch_record # preserves a copy

pitch_record[, 1:120] <- apply(apply(pitch_record[, 1:120], 2, as.character), 2, as.numeric)
pitch_record[, 121:160] <- apply(pitch_record[, 121:160], 2, as.character)
pitch_record[, 161:200] <- apply(apply(pitch_record[, 161:200], 2, as.character), 2, as.logical)
pitch_record[which(pitch_record == "NA", arr.ind = TRUE)] <- NA

pitch_record <- cbind(raw_pitch_record[, 1:2], pitch_record)

# clean_pitches and pitch_record can be joined through the following command:
pitch_full <- cbind(clean_pitches, pitch_record[, -1:-2])

# pitch_full has player and team data with the complete record of pitches thrown
# pitch_full and clean_df are same thing, but pitch_full has all pitches expanded
