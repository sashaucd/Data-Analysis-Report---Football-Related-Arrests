library(dplyr)
library(tidyr)
library("stringr")

# Data Soruces: 
# Crimes around football matches - https://www.gov.uk/government/statistics/football-related-arrests-and-banning-orders-2023-to-2024-season
# Rankings - https://github.com/jfjelstul/englishfootball/tree/master


# Read the datasets
rankings <- read.csv("rankings_filtered.csv")
matches <- read.csv("fb_arrests_monthly_gdp.csv")

View(rankings)
View(matches)


# Function to standardize team names
standardize_team_name <- function(name) {
  name %>%
    # Replace underscores with spaces
    str_replace_all("_", " ") %>%
    # Capitalize first letter of each word
    str_to_title() %>%
    # Trim any extra spaces
    str_trim()
}
# Clean and prepare the rankings data
rankings_clean <- rankings %>%
  select(season_id, team_name, position) %>%
  rename(ranking = position) %>%
  # Standardize team names in rankings
  mutate(team_name_std = standardize_team_name(team_name))
View(rankings_clean)


# Clean and prepare the matches data
matches_clean <- matches %>%
  mutate(
    season_numeric = (as.numeric(sub(".*/", "", season)) - 1),
    season_id = paste0("S-20", season_numeric, "-", 
                      case_when(
                        competition == "premier_league" ~ "1",
                        competition == "championship" ~ "2",
                        competition == "league_one" ~ "3",
                        competition == "league_two" ~ "4",
                        TRUE ~ NA_character_
                      )),
    # Standardize team names in matches
    home_team_std = standardize_team_name(home_team),
    away_team_std = standardize_team_name(away_team)
  )
  View(matches_clean)



# Join home team rankings
matches_with_rankings <- matches_clean %>%
  left_join(
    rankings_clean,
    by = c("season_id" = "season_id", "home_team_std" = "team_name_std")
  ) %>%
  rename(home_team_ranking = ranking)

  View(matches_with_rankings)


# Join away team rankings
final_dataset <- matches_with_rankings %>%
  left_join(
    rankings_clean,
    by = c("season_id" = "season_id", "away_team_std" = "team_name_std")
  ) %>%
  rename(away_team_ranking = ranking)

View(final_dataset)


# Select relevant columns and clean up
final_dataset <- final_dataset %>%
  select(
    season, competition, 
    home_team, away_team,
    home_team_ranking, away_team_ranking,
    everything(),
    -season_numeric,
    -home_team_std,
    -away_team_std,
    -contains("team_name")  # Remove the original team name columns from rankings
  )

  View(final_dataset)


# Check for unmatched teams
unmatched_teams <- final_dataset %>%
  filter(is.na(home_team_ranking) | is.na(away_team_ranking)) %>%
  select(season_id, home_team, away_team, home_team_ranking, away_team_ranking) %>%
  distinct()
View(unmatched_teams)
# Print summary statistics
summary_stats <- final_dataset %>%
  summarise(
    total_rows = n(),
    rows_with_home_ranking = sum(!is.na(home_team_ranking)),
    rows_with_away_ranking = sum(!is.na(away_team_ranking)),
    rows_with_both_rankings = sum(!is.na(home_team_ranking) & !is.na(away_team_ranking))
  )

print("Summary Statistics:")
print(summary_stats)

print("\nUnmatched Teams:")
print(unmatched_teams)

# Save the final dataset
write.csv(final_dataset, "final_dataset_with_rankings.csv", row.names = FALSE)